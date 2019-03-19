#' geometric_mean
#'
#' @param x [numeric] A \code{numeric} or \code{vector(numeric)}.
#'
#' @keywords internal
#'
#' @return [numeric]
geometric_mean <- function(x) {
  x[x == 0] <- 1
  exp(mean(log(x)))
}
