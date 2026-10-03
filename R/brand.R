#' NACHO brand colours
#'
#' The same values as `inst/brand/_brand.yml`; a test keeps them in step.
#'
#' @noRd
nacho_palette <- c(
  navy = "#182430",
  night = "#111821",
  rust = "#B64326",
  amber = "#FCB448",
  yellow = "#F0D83C",
  orange = "#E45430",
  rose = "#D85460"
)

#' Path of a file in the NACHO brand directory
#'
#' @param ... Path components under `inst/brand/`.
#'
#' @return The full path of the file.
#'
#' @noRd
brand_path <- function(...) {
  path <- system.file("brand", ..., package = "NACHO")
  if (!nzchar(path)) {
    nacho_abort(
      "The brand file {.file {file.path('brand', ...)}} is missing from the installed package.",
      class = "missing_file"
    )
  }
  path
}
