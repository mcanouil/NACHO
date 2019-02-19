#' qc_pca
#'
#' @param counts [data.frame]
#' @param n_comp [numeric]
#'
#' @return [list(data.frame)]
qc_pca <- function(counts, n_comp = 10) {
  pc <- stats::prcomp(log(counts + 1))
  pc_sum <- summary(pc)
  pc <- pc$rotation[, seq(n_comp)]
  list("pc" = pc, "pcsum" = pc_sum$importance[, seq(n_comp)])
}
