#' qc_imaging
#'
#' @param fov_counted [numeric]
#' @param fov_count [numeric]
#'
#' @return [numeric]
qc_imaging <- function(fov_counted, fov_count) {
  fov <- (fov_counted / fov_count) * 100
  round(fov, 2)
}