#' qc_imaging
#'
#' @param fov_counted
#' @param fov_count
#'
#' @return
#' @export
#'
#' @examples
qc_imaging <- function(fov_counted, fov_count) {
  fov <- (fov_counted / fov_count) * 100
  return(round(fov, 2))
}