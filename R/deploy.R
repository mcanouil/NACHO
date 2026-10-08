#' Deploy (copy) the Shiny application to the specified directory
#'
#' @param directory [[character]] A character vector of one path to the new location.
#' @param app_name [[character]] A character vector defining the Shiny application name in the new location.
#'
#' @details The deployed app calls `NACHO::nacho_app()`, so the server needs
#'   NACHO installed.
#'   The app sets the `nacho.plot_workers` option to `1` when the option is
#'   not set, so it starts one background process for the interactive plots.
#'   That process and the plot cache belong to the R process that runs the
#'   app, and each process keeps a copy of the study, so its memory grows with
#'   the size of the study (about 170 MB for `GSE74821`).
#'   To change the number of processes, set the option before the app starts,
#'   for example in `.Rprofile` on the server.
#'   See the "Plot workers" section of [nacho_app()].
#'
#' @return [[logical]] A logical indicating whether the deployment is successful (`TRUE`) or not (`FALSE`).
#' @export
#'
#' @examples
#'
#' deploy(directory = tempdir())
#'
#' if (interactive()) {
#'   shiny::runApp("NACHO")
#' }
#'
deploy <- function(directory, app_name = "NACHO") {
  if (missing(directory)) {
    nacho_abort("{.arg directory} must be provided.", class = "bad_argument")
  }
  check_string(directory)
  check_string(app_name)

  dir.create(
    file.path(directory, app_name),
    showWarnings = FALSE,
    recursive = TRUE
  )
  all(file.copy(
    from = list.files(system.file("app", package = "NACHO"), full.names = TRUE),
    to = file.path(directory, app_name),
    overwrite = TRUE,
    recursive = TRUE
  ))
}
