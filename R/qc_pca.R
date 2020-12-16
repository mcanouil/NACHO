#' qc_pca
#'
#' @param counts [[data.frame]] A `data.frame` with the count data.
#' @inheritParams load_rcc
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[list]]
qc_pca <- function(counts, n_comp = 10) {
  pc <- stats::prcomp(log(counts + 1))
  pc_sum <- summary(pc)
  pc <- pc$rotation[, seq_len(n_comp)]
  list("pc" = pc, "pcsum" = pc_sum$importance[, seq_len(n_comp)])
}
