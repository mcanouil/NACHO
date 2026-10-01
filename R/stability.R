#' @include qc-table.R
NULL

#' Standard deviation of every pairwise log ratio
#'
#' @param log_expr Samples by genes, log2 values, no missing values.
#'
#' @noRd
pairwise_sd <- function(log_expr) {
  covariance <- stats::cov(log_expr)
  variance <- diag(covariance)
  sqrt(pmax(outer(variance, variance, "+") - 2 * covariance, 0))
}

#' geNorm stability M of each gene
#'
#' The mean standard deviation of its log ratios with every other gene
#' (Vandesompele et al. 2002, Genome Biology 3, research0034).
#'
#' @noRd
genorm_m <- function(log_expr) {
  spread <- pairwise_sd(log_expr)
  rowSums(spread) / (ncol(log_expr) - 1)
}

#' geNorm ranking and pairwise variation
#'
#' Drops the gene with the largest M until two are left.
#' `V` for `"n/n+1"` is the standard deviation of the log ratio between the
#' normalisation factors from the `n` and `n + 1` most stable genes.
#'
#' @noRd
genorm_ranking <- function(log_expr, call = rlang::caller_env()) {
  if (ncol(log_expr) < 3) {
    nacho_abort(
      "geNorm needs at least three genes, not {ncol(log_expr)}.",
      class = "bad_argument",
      call = call
    )
  }
  spread <- pairwise_sd(log_expr)
  genes <- colnames(log_expr)
  n <- length(genes)
  ranking <- character(n)
  pair <- character(0)
  variation <- numeric(0)
  current <- seq_len(n)
  while (length(current) > 2) {
    m <- rowSums(spread[current, current, drop = FALSE]) / (length(current) - 1)
    worst <- current[which.max(m)]
    kept <- setdiff(current, worst)
    ratio <- rowMeans(log_expr[, kept, drop = FALSE]) -
      rowMeans(log_expr[, current, drop = FALSE])
    pair <- c(pair, sprintf("%d/%d", length(kept), length(current)))
    variation <- c(variation, stats::sd(ratio))
    ranking[length(current)] <- genes[worst]
    current <- kept
  }
  ranking[seq_len(2)] <- genes[current]
  list(ranking = ranking, pairwise_v = data.frame(pair = pair, V = variation))
}
