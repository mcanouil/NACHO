#' qc_limit_detection
#'
#' @param pos_e [numeric]
#' @param negatives [vector(numeric)]
#'
#' @return [numeric]
#'
#' @importFrom stats sd
qc_limit_detection <- function(pos_e, negatives) {
  z_score <- pos_e - mean(negatives) / stats::sd(negatives)
  return(round(z_score, 2))
}