#' geometric_mean
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
geometric_mean <- function(x) {
  x[x == 0] <- 1
  return(exp(mean(log(x))))
}
