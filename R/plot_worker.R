#' @include mod_qc_plot.R
NULL

#' Give the key of a card in the session cache
#'
#' The key changes when any input of the card changes: the object, the plot
#' type, the display options, dark mode, or the size of the card.
#'
#' @noRd
plot_cache_key <- function(object, type, options, dark, size) {
  rlang::hash(list(object, type, options, dark, size))
}

#' Build the content of one card
#'
#' An interactive card gives a girafe widget at the size of the card.
#' A static card gives the ggplot.
#' The function uses only its arguments, so it gives the same result in a mirai
#' daemon and in the app process.
#' The selection is not part of the result: the app adds it when it renders the
#' widget, so a cached widget can show any selection.
#'
#' @noRd
build_card <- function(object, type, options, dark, interactive, size) {
  plot <- app_plot(object, type, options, dark, interactive)
  if (interactive) {
    app_girafe(plot, size[["width"]], size[["height"]])
  } else {
    plot
  }
}

#' Give the number of daemons that build the plots of a session
#'
#' One core stays free for the app, and four daemons are enough for the
#' largest page, which has five cards.
#'
#' @noRd
plot_worker_count <- function(cores = parallel::detectCores()) {
  if (length(cores) != 1 || is.na(cores)) {
    return(1L)
  }
  as.integer(min(4L, max(1L, cores - 1L)))
}

#' Start the daemons that build the plots of a session
#'
#' Each session has its own mirai compute profile, so the plots of one user do
#' not wait for the plots of another user.
#' The daemons use the installed NACHO package, not the loaded one, so
#' reinstall NACHO after you change the plot code.
#' Each daemon loads NACHO and builds the ggiraph font set at once, so the first
#' page does not wait for that work.
#' The daemons stop when the session ends.
#'
#' @return The name of the compute profile, or `NULL` when mirai is not
#'   installed.
#'
#' @noRd
plot_workers_start <- function(session) {
  if (!has_package("mirai")) {
    return(NULL)
  }
  if (is.null(session$userData$plot_workers)) {
    profile <- paste0("nacho-plots-", session$token)
    libs <- .libPaths()
    mirai::daemons(
      plot_worker_count(),
      dispatcher = TRUE,
      .compute = profile
    )
    session$userData$plot_workers <- profile
    session$onSessionEnded(function() plot_workers_stop(session))
    mirai::everywhere(
      {
        .libPaths(libs)
        asNamespace("NACHO")[["girafe_font_set"]]()
      },
      libs = libs,
      .compute = profile
    )
  }
  session$userData$plot_workers
}

#' Stop the daemons of a session
#'
#' A build that still runs stops with its daemon.
#'
#' @noRd
plot_workers_stop <- function(session) {
  profile <- session$userData$plot_workers
  if (!is.null(profile)) {
    mirai::daemons(0, .compute = profile)
    session$userData$plot_workers <- NULL
  }
  invisible(NULL)
}

#' Create the task that builds one card in a daemon
#'
#' `jobs` is an environment that the card owns.
#' `jobs$wanted` holds the key of the build that the card waits for, and the
#' task records the running mirai in `jobs$running`, so the card can stop it.
#' The task skips a call whose key is no longer wanted.
#' This happens when the inputs change again while an earlier build runs.
#' The task never fails: it gives `NULL` for a skipped call, and a list with the
#' key and either the value or the error otherwise.
#' A failure in ExtendedTask writes a warning to the console, which a stopped
#' build must not do.
#' After the session ends, the task does not settle, so it does not write to the
#' reactive values of a session that no longer exists.
#'
#' @noRd
card_task <- function(session, profile, jobs) {
  libs <- .libPaths()
  shiny::ExtendedTask$new(
    function(key, object, type, options, dark, interactive, size) {
      if (!identical(key, jobs$wanted)) {
        return(NULL)
      }
      running <- mirai::mirai(
        {
          .libPaths(libs)
          asNamespace("NACHO")[["build_card"]](
            object,
            type,
            options,
            dark,
            interactive,
            size
          )
        },
        libs = libs,
        object = object,
        type = type,
        options = options,
        dark = dark,
        interactive = interactive,
        size = size,
        .compute = profile
      )
      jobs$running <- running
      promises::promise(function(resolve, reject) {
        promises::then(
          running,
          onFulfilled = function(value) {
            if (!session$isClosed()) resolve(list(key = key, value = value))
          },
          onRejected = function(error) {
            if (!session$isClosed()) resolve(list(key = key, error = error))
          }
        )
      })
    }
  )
}

#' Give the message of a failed build
#'
#' @noRd
build_error_message <- function(error) {
  if (inherits(error, "condition")) {
    return(conditionMessage(error))
  }
  paste(as.character(error), collapse = "\n")
}

#' Give the widget of an interactive card, built once per key
#'
#' `inputs` is a reactive that gives the arguments of `build_card()`.
#' Built widgets go into the session cache, so a card that comes back to
#' earlier inputs, such as a return from full screen, does not build again.
#'
#' Without `workers`, the card builds in the app process when its output asks
#' for the widget, as hidden outputs do not ask.
#'
#' With `workers`, the card builds in a daemon through `card_task()`, and only
#' while `active()` is `TRUE`, that is while its page shows.
#' When the page changes, the card stops its build and drops its result.
#' While a build runs, the output shows that it is in progress and keeps the
#' last widget.
#'
#' @noRd
card_widget <- function(session, inputs, workers, active) {
  key <- shiny::reactive({
    args <- inputs()
    plot_cache_key(args$object, args$type, args$options, args$dark, args$size)
  })
  if (is.null(workers)) {
    return(shiny::reactive({
      wanted <- key()
      if (session$cache$exists(wanted)) {
        return(session$cache$get(wanted))
      }
      value <- do.call(build_card, inputs())
      session$cache$set(wanted, value)
      value
    }))
  }
  jobs <- new.env(parent = emptyenv())
  task <- card_task(session, workers, jobs)
  shown <- shiny::reactiveVal(NULL)
  pending <- shiny::reactiveVal(FALSE)
  stop_running <- function() {
    if (inherits(jobs$running, "mirai") && mirai::unresolved(jobs$running)) {
      mirai::stop_mirai(jobs$running)
    }
  }
  drop_build <- function() {
    jobs$wanted <- NULL
    stop_running()
    pending(FALSE)
  }
  shiny::observe(priority = 1, {
    shiny::req(active())
    wanted <- key()
    if (identical(shiny::isolate(shown())$key, wanted)) {
      drop_build()
      return(invisible(NULL))
    }
    if (session$cache$exists(wanted)) {
      drop_build()
      shown(list(key = wanted, value = session$cache$get(wanted)))
      return(invisible(NULL))
    }
    if (identical(jobs$wanted, wanted)) {
      return(invisible(NULL))
    }
    stop_running()
    jobs$wanted <- wanted
    pending(TRUE)
    do.call(task$invoke, c(list(key = wanted), inputs()))
  })
  shiny::observeEvent(
    active(),
    if (!active()) drop_build(),
    ignoreInit = TRUE
  )
  shiny::observeEvent(task$status(), {
    done <- if (identical(task$status(), "success")) task$result()
    if (is.null(done)) {
      return(invisible(NULL))
    }
    if (is.null(done$error)) {
      session$cache$set(done$key, done$value)
    }
    if (identical(done$key, jobs$wanted)) {
      jobs$wanted <- NULL
      shown(done)
      pending(FALSE)
    }
  })
  shiny::reactive({
    if (pending()) {
      shiny::req(FALSE, cancelOutput = "progress")
    }
    card <- shiny::req(shown())
    if (!is.null(card$error)) {
      stop(build_error_message(card$error), call. = FALSE)
    }
    card$value
  })
}
