#' geometric_mean
#'
#' @param x [numeric] A `numeric` or a `numeric` vector.
#'
#' @keywords internal
#' @usage NULL
#'
#' @return [numeric]
geometric_mean <- function(x) {
  x[x == 0] <- 1
  exp(mean(log(x)))
}
