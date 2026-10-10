#' @include mod_qc_plot.R
NULL

#' Give the key of a card in the app cache
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

#' Give the number of daemons that build the plots
#'
#' One core stays free for the app, and four daemons are enough for the
#' largest page, which has five cards.
#' The option `nacho.plot_workers` caps the number, and `0` turns the daemons
#' off.
#'
#' @noRd
plot_worker_count <- function(
  cores = parallel::detectCores(),
  cap = getOption("nacho.plot_workers")
) {
  count <- if (length(cores) != 1 || is.na(cores)) {
    1L
  } else {
    as.integer(min(4L, max(1L, cores - 1L)))
  }
  if (is.numeric(cap) && length(cap) == 1 && !is.na(cap)) {
    count <- as.integer(max(0L, min(count, floor(cap))))
  }
  count
}

plot_pool <- new.env(parent = emptyenv())
plot_pool$timeout <- 30000
plot_pool$generation <- 0L

#' Start the daemons that build the plots of the app
#'
#' One pool serves every session of the R process.
#' The first session that shows interactive plots starts it, and it stops when
#' the app stops.
#' The function does not wait for the daemons: they connect in the background,
#' and builds wait in the queue until a daemon is free.
#' The daemons start with the library paths of the app, so they find the
#' packages the app uses.
#' They use the installed NACHO package, not the loaded one, so reinstall NACHO
#' after you change the plot code.
#' When all daemons are connected, each one loads NACHO and builds the ggiraph
#' font set, so the first page does not wait for that work.
#' When the daemons cannot start, the plots build in the app process.
#' Each pool has its own profile name, so a callback of a stopped pool, such
#' as a late probe or warm-up, does nothing to the next pool.
#'
#' @return The name of the compute profile, or `NULL` when the plots build in
#'   the app process: mirai is not installed, the option `nacho.plot_workers`
#'   is `0`, or the daemons did not start.
#'
#' @noRd
plot_workers_start <- function() {
  count <- plot_worker_count()
  if (count == 0L || !has_package("mirai") || isTRUE(plot_pool$failed)) {
    return(NULL)
  }
  if (is.null(plot_pool$profile)) {
    plot_pool$generation <- plot_pool$generation + 1L
    profile <- paste0("nacho-plots-", plot_pool$generation)
    plot_pool$profile <- profile
    started <- tryCatch(
      {
        with_library_paths({
          mirai::daemons(
            url = mirai::local_url(),
            dispatcher = TRUE,
            .compute = profile
          )
          mirai::launch_local(count, .compute = profile)
        })
        TRUE
      },
      error = function(cnd) {
        plot_workers_fail()
        FALSE
      }
    )
    if (!started) {
      return(NULL)
    }
    shiny::onStop(plot_workers_stop, session = NULL)
    plot_workers_probe(profile)
    plot_workers_warm(profile, count, .libPaths())
  }
  plot_pool$profile
}

#' Check that the daemons start
#'
#' The probe is a small task with a time limit, sent before any build, so it
#' does not wait behind builds.
#' When a daemon answers, the pool is warm.
#' When no daemon answers within the limit (30 seconds), the pool fails and the
#' plots build in the app process.
#' Builds have no time limit, because a large page can wait in the queue of a
#' healthy pool for longer than that.
#'
#' @noRd
plot_workers_probe <- function(profile) {
  probe <- mirai::mirai(TRUE, .timeout = plot_pool$timeout, .compute = profile)
  promises::then(
    probe,
    onFulfilled = function(value) {
      if (identical(plot_pool$profile, profile)) plot_pool$warm <- TRUE
    },
    onRejected = function(error) {
      if (identical(plot_pool$profile, profile)) plot_workers_fail()
    }
  )
  invisible(NULL)
}

#' Load NACHO and the font set in each daemon once all have connected
#'
#' The check repeats every quarter of a second for at most 30 seconds, without
#' blocking the app.
#'
#' @noRd
plot_workers_warm <- function(profile, count, libs, tries = 120L) {
  if (!identical(plot_pool$profile, profile) || isTRUE(plot_pool$failed)) {
    return(invisible(NULL))
  }
  if (mirai::status(.compute = profile)$connections >= count) {
    mirai::everywhere(
      {
        .libPaths(libs)
        asNamespace("NACHO")[["girafe_font_set"]]()
      },
      libs = libs,
      .compute = profile
    )
  } else if (tries > 0L) {
    later::later(
      function() plot_workers_warm(profile, count, libs, tries - 1L),
      0.25
    )
  }
  invisible(NULL)
}

#' Stop the daemons of the app
#'
#' A build that still runs stops with its daemon.
#' After a stop, the next session starts a new pool.
#'
#' @noRd
plot_workers_stop <- function() {
  if (!is.null(plot_pool$profile)) {
    mirai::daemons(0, .compute = plot_pool$profile)
  }
  plot_pool$profile <- NULL
  plot_pool$failed <- NULL
  plot_pool$warm <- NULL
  invisible(NULL)
}

#' Give up on daemons that are unavailable
#'
#' This covers daemons that did not start and daemons that stopped.
#'
#' The pool stops, and the plots build in the app process until the app
#' stops.
#' The flag is set first, so builds that the stop rejects build in the app
#' process.
#'
#' @noRd
plot_workers_fail <- function() {
  if (isTRUE(plot_pool$failed)) {
    return(invisible(NULL))
  }
  plot_pool$failed <- TRUE
  if (!is.null(plot_pool$profile)) {
    mirai::daemons(0, .compute = plot_pool$profile)
  }
  nacho_inform(
    "The plot workers are unavailable, so the app builds the plots itself."
  )
  invisible(NULL)
}

#' Tell whether every daemon of a warm pool has gone
#'
#' A pool is warm once a daemon has answered the start probe.
#' When none is connected after that, a build sent to the pool would wait in
#' the queue for ever.
#'
#' @noRd
pool_lost <- function(profile) {
  isTRUE(plot_pool$warm) &&
    identical(plot_pool$profile, profile) &&
    mirai::status(.compute = profile)$connections == 0L
}

#' Build one card in the app process for a task
#'
#' @noRd
build_in_process <- function(build, key, args, cache) {
  tryCatch(
    {
      value <- do.call(build_card, args)
      cache$set(key, value)
      list(build = build, key = key, value = value)
    },
    error = function(error) list(build = build, key = key, error = error)
  )
}

#' Create the task that builds one card in a daemon
#'
#' `jobs` is an environment that the card owns.
#' `jobs$build` counts the builds the card asked for, and the task records the
#' running mirai in `jobs$running`, so the card can stop it.
#' The task skips a call that is not the last build asked for.
#' This happens when the inputs change again while an earlier build runs.
#' A finished build goes into `cache`, even when the card no longer waits for
#' it.
#'
#' The task never fails.
#' It gives `NULL` for a skipped, stopped or interrupted build, and otherwise a list with the
#' build number, the key, and either the value or the error.
#' A failure in ExtendedTask writes a warning to the console, which a stopped
#' build must not do.
#' After the session ends, the task does not settle, so it does not write to the
#' reactive values of a session that no longer exists.
#'
#' When the daemon of a build stops, or no daemon is left, the pool fails and
#' the build runs in the app process.
#'
#' @noRd
card_task <- function(session, profile, jobs, cache) {
  libs <- .libPaths()
  shiny::ExtendedTask$new(function(build, key, args) {
    if (!identical(build, jobs$build)) {
      return(NULL)
    }
    if (!isTRUE(plot_pool$failed) && pool_lost(profile)) {
      plot_workers_fail()
    }
    if (isTRUE(plot_pool$failed)) {
      return(build_in_process(build, key, args, cache))
    }
    running <- mirai::mirai(
      {
        .libPaths(libs)
        do.call(asNamespace("NACHO")[["build_card"]], args)
      },
      libs = libs,
      args = args,
      .compute = profile
    )
    jobs$running <- running
    promises::promise(function(resolve, reject) {
      promises::then(
        running,
        onFulfilled = function(value) {
          if (mirai::is_mirai_interrupt(value)) {
            if (!session$isClosed()) {
              resolve(NULL)
            }
            return(invisible(NULL))
          }
          cache$set(key, value)
          if (!session$isClosed()) {
            resolve(list(build = build, key = key, value = value))
          }
        },
        onRejected = function(error) {
          if (session$isClosed()) {
            return(invisible(NULL))
          }
          code <- running$data
          build_error <- inherits(code, "miraiError") ||
            !inherits(code, "errorValue")
          result <- if (build_error) {
            list(build = build, key = key, error = error)
          } else if (build_cancelled(code)) {
            NULL
          } else {
            if (identical(plot_pool$profile, profile)) {
              plot_workers_fail()
            }
            build_in_process(build, key, args, cache)
          }
          resolve(result)
        }
      )
    })
  })
}

#' Tell whether a build ended because it was stopped or interrupted
#'
#' `mirai::stop_mirai()` gives the error value 20, and an interrupt in the
#' daemon gives a `miraiInterrupt`, which has no number.
#'
#' @noRd
build_cancelled <- function(code) {
  mirai::is_mirai_interrupt(code) || identical(as.integer(code), 20L)
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
#' Built widgets go into the app cache (the session cache when the app has
#' none), so a card that comes back to earlier inputs, such as a return from
#' full screen, another tab or a reload, does not build again.
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
  cache <- shiny::getShinyOption("cache", default = session$cache)
  if (is.null(workers)) {
    return(shiny::reactive({
      wanted <- key()
      if (cache$exists(wanted)) {
        return(cache$get(wanted))
      }
      value <- do.call(build_card, inputs())
      cache$set(wanted, value)
      value
    }))
  }
  jobs <- new.env(parent = emptyenv())
  jobs$build <- 0L
  task <- card_task(session, workers, jobs, cache)
  shown <- shiny::reactiveVal(NULL)
  pending <- shiny::reactiveVal(FALSE)
  stop_running <- function() {
    if (inherits(jobs$running, "mirai") && mirai::unresolved(jobs$running)) {
      mirai::stop_mirai(jobs$running)
    }
  }
  session$onSessionEnded(stop_running)
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
    if (cache$exists(wanted)) {
      drop_build()
      shown(list(key = wanted, value = cache$get(wanted)))
      return(invisible(NULL))
    }
    if (identical(jobs$wanted, wanted)) {
      return(invisible(NULL))
    }
    stop_running()
    jobs$wanted <- wanted
    jobs$build <- jobs$build + 1L
    pending(TRUE)
    task$invoke(jobs$build, wanted, inputs())
  })
  shiny::observeEvent(
    active(),
    if (!active()) drop_build(),
    ignoreInit = TRUE
  )
  shiny::observeEvent(task$status(), {
    done <- if (identical(task$status(), "success")) task$result()
    if (
      is.null(done) ||
        is.null(jobs$wanted) ||
        !identical(done$build, jobs$build)
    ) {
      return(invisible(NULL))
    }
    jobs$wanted <- NULL
    shown(done)
    pending(FALSE)
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
