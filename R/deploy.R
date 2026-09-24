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
#' deploy(directory = ".")
#'
#' if (interactive()) {
#'   shiny::runApp("NACHO")
#' }
#'
deploy <- function(directory = "/srv/shiny-server", app_name = "NACHO") {
  dir.create(file.path(directory, app_name), showWarnings = FALSE, recursive = TRUE)
  all(file.copy(
    from = list.files(system.file("app", package = "NACHO"), full.names = TRUE),
    to = file.path(directory, app_name),
    overwrite = TRUE, recursive = TRUE
  ))
}
