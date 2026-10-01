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

#' NormFinder stability of each gene
#'
#' Andersen et al. 2004, Cancer Research 64, 5245.
#' With groups, the stability combines the variation inside the groups and
#' the variation between the groups.
#' Without groups, it is the standard deviation of the residual variation of
#' the gene.
#'
#' @param group A factor with one value per sample, or `NULL`.
#'
#' @noRd
normfinder_rho <- function(log_expr, group = NULL, call = rlang::caller_env()) {
  k <- ncol(log_expr)
  if (k < 3) {
    nacho_abort(
      "NormFinder needs at least three genes, not {k}.",
      class = "bad_argument",
      call = call
    )
  }
  residual_variance <- function(x) {
    sample_means <- rowMeans(x)
    gene_means <- colMeans(x)
    residuals <- t(t(x - sample_means) - gene_means) + mean(sample_means)
    a <- colSums(residuals^2) / (nrow(x) - 1)
    (a - sum(a) / (k * (k - 1))) / (1 - 2 / k)
  }
  if (is.null(group) || nlevels(factor(group)) == 1) {
    return(sqrt(pmax(residual_variance(log_expr), 0)))
  }
  group <- factor(group)
  sizes <- as.integer(table(group))
  if (any(sizes < 2)) {
    nacho_abort(
      "NormFinder needs at least two samples in each group.",
      class = "bad_argument",
      call = call
    )
  }
  group_means <- rowsum(log_expr, group) / sizes
  variance <- t(vapply(
    levels(group),
    function(level) {
      pmax(residual_variance(log_expr[group == level, , drop = FALSE]), 0)
    },
    numeric(k)
  ))
  m <- nlevels(group)
  difference <- t(
    t(group_means - rowMeans(group_means)) - colMeans(group_means)
  ) +
    mean(group_means)
  va <- variance / sizes
  tau <- max(sum(difference^2) / ((m - 1) * (k - 1)) - mean(va), 0)
  shrunk <- difference * tau / (tau + va)
  shrunk_variance <- va + tau * va / (tau + va)
  rho <- colMeans(abs(shrunk) + sqrt(shrunk_variance))
  stats::setNames(rho, colnames(log_expr))
}
