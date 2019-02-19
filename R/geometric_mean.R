#' geometric_mean
#'
#' @param x [numeric]
#'
#' @return [numeric]
geometric_mean <- function(x) {
  x[x == 0] <- 1
  exp(mean(log(x)))
}
