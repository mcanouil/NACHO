#' qc_imaging
#'
#' @param fov_counted [numeric] Values from "lane_FovCounted" in the RCC files.
#' @param fov_count [numeric] Values from "lane_FovCount" in the RCC files.
#'
#' @return [numeric]
qc_imaging <- function(fov_counted, fov_count) {
  fov <- (fov_counted / fov_count) * 100
  round(fov, 2)
}