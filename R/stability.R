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

check_enough_samples <- function(log_expr, method, call) {
  if (nrow(log_expr) < 2) {
    nacho_abort(
      "{method} needs at least two samples, not {nrow(log_expr)}.",
      class = "bad_argument",
      call = call
    )
  }
}

check_enough_genes <- function(log_expr, method, call) {
  if (ncol(log_expr) < 3) {
    nacho_abort(
      "{method} needs at least three genes, not {ncol(log_expr)}.",
      class = "bad_argument",
      call = call
    )
  }
}

#' geNorm stability M of each gene
#'
#' The mean standard deviation of its log ratios with every other gene
#' (Vandesompele et al. 2002, Genome Biology 3, research0034).
#'
#' @noRd
genorm_m <- function(log_expr, call = rlang::caller_env()) {
  check_enough_samples(log_expr, "geNorm", call)
  check_enough_genes(log_expr, "geNorm", call)
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
  check_enough_samples(log_expr, "geNorm", call)
  check_enough_genes(log_expr, "geNorm", call)
  if (is.null(colnames(log_expr))) {
    nacho_abort(
      "geNorm needs column names to name the genes.",
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
  totals <- rowSums(spread)
  while (length(current) > 2) {
    worst <- current[which.max(totals[current])]
    totals <- totals - spread[, worst]
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
  check_enough_samples(log_expr, "NormFinder", call)
  check_enough_genes(log_expr, "NormFinder", call)
  k <- ncol(log_expr)
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
  weight <- ifelse(tau + va > 0, tau / (tau + va), 0)
  shrunk <- difference * weight
  shrunk_variance <- va + weight * va
  rho <- colMeans(abs(shrunk) + sqrt(shrunk_variance))
  stats::setNames(rho, colnames(log_expr))
}

#' Log2 counts of genes fit for stability analysis
#'
#' Keeps the genes detected in at least `min_detection` of the samples and
#' with no missing count.
#' Per-sample scaling cancels out of both geNorm and NormFinder, so raw counts
#' give the same ranking as positive-normalised ones.
#' Background correction is deliberately skipped, so the ranking does not
#' depend on the background setting.
#'
#' @noRd
stability_input <- function(counts, detection_rate, min_detection) {
  keep <- !is.na(detection_rate) &
    detection_rate >= min_detection &
    rowSums(is.na(counts)) == 0
  t(log2(pmax(counts[keep, , drop = FALSE], 1)))
}

#' Rank genes by expression stability
#'
#' Ranks candidate reference genes with geNorm (Vandesompele et al. 2002) and
#' NormFinder (Andersen et al. 2004), after keeping the genes detected in most
#' samples.
#' With `group`, NormFinder accounts for the groups, and a Kruskal-Wallis test
#' tells whether each gene's expression differs between them, which makes a
#' poor reference gene.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @param genes The genes to rank; `NULL` ranks the `Housekeeping` probes.
#' @param group A column of `nacho_samples(x)` with the biological groups, or
#'   `NULL`.
#' @param min_detection The smallest share of samples in which a gene must be
#'   above background to be ranked.
#'
#' @details
#' The two most stable genes share the same geNorm M by construction, so their
#' order, ranks 1 and 2, is arbitrary.
#' Without `group`, `NormFinder_rho` is the standard deviation of the
#' residual variation of the gene.
#' With `group`, it also counts the variation between the groups, shrunk
#' towards zero as NormFinder does.
#'
#' @return A list with `ranking`, a data frame with one row per gene from the
#'   most to the least stable: `Name`, `CodeClass`, `detection_rate`,
#'   `mean_log2`, `geNorm_M`, `geNorm_rank`, `NormFinder_rho` and, with
#'   `group`, `group_p_value` and `group_p_adjusted` (Benjamini-Hochberg); and
#'   `pairwise_v`, the geNorm pairwise variation `V` for each number of genes.
#'   A `V` below 0.15 is the usual sign that adding a gene no longer helps.
#' @export
#' @examples
#' data(GSE74821)
#' housekeeping_stability(GSE74821)$ranking
housekeeping_stability <- function(
  x,
  genes = NULL,
  group = NULL,
  min_detection = 0.9
) {
  check_nacho(x)
  check_character(genes, allow_null = TRUE)
  check_string(group, allow_null = TRUE)
  check_proportion(min_detection)
  if (!is.null(group)) {
    check_column(group, nacho_samples(x), data_arg = "nacho_samples(x)")
  }
  probes <- x@probes
  genes <- genes %||% probes[["Name"]][probes[["CodeClass"]] == "Housekeeping"]
  unknown <- setdiff(genes, probes[["Name"]])
  if (length(unknown) > 0) {
    nacho_abort(
      "{.arg genes} has name{?s} that {?is/are} not a probe: {.val {utils::head(unknown, 5)}}.",
      class = "bad_argument"
    )
  }
  if (length(genes) == 0) {
    nacho_abort(
      c(
        "There are no genes to rank.",
        i = "Pass {.arg genes}, since the object has no {.val Housekeeping} probe."
      ),
      class = "bad_argument"
    )
  }
  rows <- match(unique(genes), probes[["Name"]])
  if (all(is.na(probes[["detection_rate"]][rows]))) {
    nacho_abort(
      c(
        "No sample has a detection limit, so {.fn housekeeping_stability} cannot tell which genes are detected.",
        i = "A detection limit needs two kept negative probes with counts."
      ),
      class = "no_detection_rate"
    )
  }
  log_expr <- stability_input(
    x@counts[rows, , drop = FALSE],
    probes[["detection_rate"]][rows],
    min_detection
  )
  if (ncol(log_expr) < 3) {
    nacho_abort(
      c(
        paste(
          "Ranking needs at least three genes detected in",
          "{min_detection * 100}% of samples, and {ncol(log_expr)} {?is/are}."
        ),
        i = "Pass more {.arg genes}, or lower {.arg min_detection}."
      ),
      class = "bad_argument"
    )
  }
  groups <- NULL
  if (!is.null(group)) {
    values <- nacho_samples(x)[[group]]
    if (anyNA(values)) {
      nacho_abort(
        c(
          "{.arg group} must have no missing values.",
          x = "Column {.field {group}} has {sum(is.na(values))} missing value{?s}."
        ),
        class = "bad_argument"
      )
    }
    groups <- factor(values)
    if (nlevels(groups) < 2) {
      nacho_abort(
        "{.arg group} must have at least two levels, and {.field {group}} has {nlevels(groups)}.",
        class = "bad_argument"
      )
    }
  }
  genorm <- genorm_ranking(log_expr)
  kept <- match(colnames(log_expr), probes[["Name"]])
  ranking <- data.frame(
    Name = colnames(log_expr),
    CodeClass = probes[["CodeClass"]][kept],
    detection_rate = probes[["detection_rate"]][kept],
    mean_log2 = unname(colMeans(log_expr)),
    geNorm_M = unname(genorm_m(log_expr)),
    geNorm_rank = match(colnames(log_expr), genorm[["ranking"]]),
    NormFinder_rho = unname(normfinder_rho(log_expr, groups))
  )
  if (!is.null(groups)) {
    ranking[["group_p_value"]] <- unname(apply(log_expr, 2, function(values) {
      stats::kruskal.test(values, groups)[["p.value"]]
    }))
    ranking[["group_p_adjusted"]] <- stats::p.adjust(
      ranking[["group_p_value"]],
      "BH"
    )
  }
  ranking <- ranking[order(ranking[["geNorm_rank"]]), , drop = FALSE]
  rownames(ranking) <- NULL
  list(ranking = ranking, pairwise_v = genorm[["pairwise_v"]])
}
