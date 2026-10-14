#' Run a NACHO function in a mirai daemon, or in the app process
#'
#' The daemon uses the installed NACHO package, not the loaded one, so
#' reinstall NACHO after you change the function.
#'
#' @param fun The name of the NACHO function.
#' @param args The list of arguments of the function.
#' @param compute The mirai compute profile that runs the function, or `NULL`
#'   to run it in the app process.
#'
#' @return A mirai, or the result of the function when `compute` is `NULL`.
#'
#' @noRd
run_in_daemon <- function(fun, args, compute = NULL) {
  if (!is.null(compute)) {
    libs <- .libPaths()
    return(mirai::mirai(
      {
        .libPaths(libs)
        do.call(asNamespace("NACHO")[[fun]], args)
      },
      fun = fun,
      args = args,
      libs = libs,
      .compute = compute
    ))
  }
  do.call(asNamespace("NACHO")[[fun]], args)
}

#' Hand a background job to an ExtendedTask
#'
#' After the session ends, the promise never settles, so the task does not
#' write to the reactive values of a session that no longer exists.
#'
#' @param running A mirai, or the result of a job that is already done.
#' @param ended A function that returns `TRUE` after the session ends.
#'
#' @noRd
settle_while_open <- function(running, ended) {
  if (!inherits(running, "mirai")) {
    return(running)
  }
  promises::promise(function(resolve, reject) {
    promises::then(
      running,
      onFulfilled = function(value) if (!ended()) resolve(value),
      onRejected = function(error) if (!ended()) reject(error)
    )
  })
}

#' Run the jobs of a task button in the background
#'
#' When mirai is installed, the session gets one mirai daemon.
#' The daemon starts, and loads NACHO, when `warm_on` first gives a value, or
#' else with the first job.
#' When the daemon cannot start, or mirai is not installed, the jobs run in the
#' app process.
#' When the session ends, the running job and the daemon stop, and the task
#' gives no more results.
#'
#' @param session The Shiny session.
#' @param button_id The id of the [bslib::input_task_button()] that starts a
#'   job.
#' @param fun The name of the NACHO function that each job runs.
#' @param on_success A function of the result of a job.
#' @param on_error A function of the error message of a job.
#' @param on_end A function that runs when the session ends.
#' @param warm_on A reactive expression whose first value starts the daemon
#'   before the first job, or `NULL`.
#'
#' @return A [shiny::ExtendedTask], whose `invoke()` takes the arguments of
#'   `fun`.
#'
#' @noRd
session_task <- function(
  session,
  button_id,
  fun,
  on_success,
  on_error,
  on_end = function() NULL,
  warm_on = NULL
) {
  profile <- basename(tempfile("nacho-task-"))
  running <- NULL
  daemon <- new.env(parent = emptyenv())
  daemon$tried <- FALSE
  daemon$on <- FALSE
  ended <- FALSE
  session$onSessionEnded(function() {
    ended <<- TRUE
    if (inherits(running, "mirai")) {
      mirai::stop_mirai(running)
    }
    if (daemon$on) {
      mirai::daemons(0, .compute = profile)
    }
    on_end()
  })
  start_daemon <- function() {
    if (daemon$tried || !has_package("mirai")) {
      return(invisible(NULL))
    }
    daemon$tried <- TRUE
    daemon$on <- tryCatch(
      {
        libs <- .libPaths()
        with_library_paths(
          mirai::daemons(1, dispatcher = TRUE, .compute = profile)
        )
        mirai::mirai(
          {
            .libPaths(libs)
            loadNamespace("NACHO")
            NULL
          },
          libs = libs,
          .compute = profile
        )
        TRUE
      },
      error = function(cnd) FALSE
    )
  }
  if (!is.null(warm_on)) {
    shiny::observeEvent(warm_on(), start_daemon(), once = TRUE)
  }
  task <- shiny::ExtendedTask$new(function(...) {
    start_daemon()
    running <<- run_in_daemon(fun, list(...), if (daemon$on) profile)
    settle_while_open(running, function() ended)
  }) |>
    bslib::bind_task_button(button_id)
  shiny::observeEvent(task$status(), {
    if (ended) {
      return(invisible(NULL))
    }
    if (task$status() == "success") {
      on_success(task$result())
    } else if (task$status() == "error") {
      on_error(tryCatch(
        task$result(),
        error = function(cnd) cli::ansi_strip(rlang::cnd_message(cnd))
      ))
    }
  })
  task
}
