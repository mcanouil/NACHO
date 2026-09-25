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
  pc <- stats::prcomp(t(log(counts + 1)))
  pc_sum <- summary(pc)
  list(
    "pc" = pc[["x"]][, seq_len(n_comp), drop = FALSE],
    "pcsum" = pc_sum[["importance"]][, seq_len(n_comp), drop = FALSE]
  )
}
