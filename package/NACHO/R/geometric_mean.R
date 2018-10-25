#' geometric_mean
#'
#' @param x [numeric]
#'
#' @return [numeric]
#'
#' @examples
geometric_mean <- function(x) {
  x[x == 0] <- 1
  return(exp(mean(log(x))))
}
