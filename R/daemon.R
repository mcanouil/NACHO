#' Run a NACHO function in a mirai daemon when mirai is installed
#'
#' The daemon uses the installed NACHO package, not the loaded one, so
#' reinstall NACHO after you change the function.
#'
#' @param fun The name of the NACHO function.
#' @param args The list of arguments of the function.
#' @param compute The mirai compute profile that runs the function.
#'
#' @return A mirai, or the result of the function when mirai is not installed.
#'
#' @noRd
run_in_daemon <- function(fun, args, compute = NULL) {
  if (has_package("mirai")) {
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
#' The first job starts one mirai daemon for the session.
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
  on_end = function() NULL
) {
  profile <- basename(tempfile("nacho-task-"))
  running <- NULL
  daemons_on <- FALSE
  ended <- FALSE
  session$onSessionEnded(function() {
    ended <<- TRUE
    if (inherits(running, "mirai")) {
      mirai::stop_mirai(running)
    }
    if (daemons_on) {
      mirai::daemons(0, .compute = profile)
    }
    on_end()
  })
  task <- shiny::ExtendedTask$new(function(...) {
    if (has_package("mirai") && !daemons_on) {
      mirai::daemons(1, dispatcher = TRUE, .compute = profile)
      daemons_on <<- TRUE
    }
    running <<- run_in_daemon(fun, list(...), profile)
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
