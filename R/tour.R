#' The HTML dependency of the vendored driver.js
#'
#' The files are driver.js 1.9.0, unchanged below their header comments.
#' The header of `inst/tour/driver.js` says how to update them.
#'
#' @noRd
driver_dependency <- function() {
  htmltools::htmlDependency(
    name = "driver.js",
    version = "1.9.0",
    src = "tour",
    package = "NACHO",
    script = "driver.js",
    stylesheet = "driver.css"
  )
}

#' The HTML dependency of the NACHO tour, which drives driver.js
#'
#' @noRd
tour_dependency <- function() {
  htmltools::htmlDependency(
    name = "nacho-tour",
    version = as.character(utils::packageVersion("NACHO")),
    src = "tour",
    package = "NACHO",
    script = "nacho-tour.js",
    stylesheet = "nacho-tour.css"
  )
}

#' Both dependencies of the tour, driver.js first
#'
#' @noRd
tour_dependencies <- function() {
  list(driver_dependency(), tour_dependency())
}
