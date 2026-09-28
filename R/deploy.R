#' Deploy (copy) the Shiny application to the specified directory
#'
#' @param directory [[character]] A character vector of one path to the new location.
#' @param app_name [[character]] A character vector defining the Shiny application name in the new location.
#'
#' @details The app needs the markdown package on the server to show its help pages.
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
