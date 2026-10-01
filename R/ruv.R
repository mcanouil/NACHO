#' @include stability.R
NULL

#' Remove unwanted variation with control genes
#'
#' RUVg (Risso et al. 2014, Nature Biotechnology 32, 896) on log values.
#' The unwanted factors `W` are the first `k` left singular vectors of the
#' centred control genes.
#' Each gene is corrected by its regression on `W`.
#' This follows `RUVSeq::RUVg(isLog = TRUE)`.
#'
#' @noRd
ruvg <- function(
  log_expr,
  controls,
  k,
  tolerance = 1e-8,
  call = rlang::caller_env()
) {
  control_expr <- log_expr[, controls, drop = FALSE]
  if (anyNA(control_expr)) {
    missing_genes <- colnames(control_expr)[colSums(is.na(control_expr)) > 0] # nolint: object_usage_linter.
    nacho_abort(
      c(
        "RUVg needs every count of its control genes.",
        x = "Missing counts in: {.val {utils::head(missing_genes, 5)}}."
      ),
      class = "bad_argument",
      call = call
    )
  }
  if (ncol(control_expr) == 0 && k > 0) {
    nacho_abort(
      c(
        "RUVg needs control genes.",
        x = "No gene is marked as a control."
      ),
      class = "bad_argument",
      call = call
    )
  }
  no_factor <- list(
    W = matrix(
      numeric(0),
      nrow(log_expr),
      0,
      dimnames = list(rownames(log_expr), NULL)
    ),
    corrected = log_expr
  )
  if (k == 0) {
    return(no_factor)
  }
  decomposition <- svd(scale(control_expr, center = TRUE, scale = FALSE))
  usable <- sum(decomposition[["d"]] > tolerance)
  if (usable == 0) {
    return(no_factor)
  }
  k <- min(k, usable)
  w <- decomposition[["u"]][, seq_len(k), drop = FALSE]
  dimnames(w) <- list(rownames(log_expr), paste0("W_", seq_len(k)))
  corrected <- log_expr
  for (gene in seq_len(ncol(log_expr))) {
    seen <- !is.na(log_expr[, gene])
    w_seen <- w[seen, , drop = FALSE]
    alpha <- solve(crossprod(w_seen), crossprod(w_seen, log_expr[seen, gene]))
    corrected[seen, gene] <- log_expr[seen, gene] - w_seen %*% alpha
  }
  list(W = w, corrected = corrected)
}

#' Mean spread of the relative log expression
#'
#' Each gene minus its median over samples.
#' The interquartile range of each sample is then averaged.
#'
#' @noRd
rle_iqr <- function(log_expr) {
  rle <- sweep(log_expr, 2, apply(log_expr, 2, stats::median, na.rm = TRUE))
  mean(apply(rle, 1, stats::IQR, na.rm = TRUE))
}

#' The log expression matrix and control flags that RUVg needs
#'
#' @param scaled The background-applied, positive-scaled counts.
#' @param probes The probe table, with `CodeClass` and `Name`.
#' @param housekeeping_genes The names of the control genes.
#'
#' @return A list: `log_expr` (samples by genes), `controls` and `rows`.
#'
#' @noRd
ruv_input_from_scaled <- function(
  scaled,
  probes,
  housekeeping_genes,
  call = rlang::caller_env()
) {
  rows <- grepl("Endogenous|Housekeeping", probes[["CodeClass"]])
  log_expr <- t(log2(scaled[rows, , drop = FALSE] + 1))
  colnames(log_expr) <- probes[["Name"]][rows]
  controls <- colnames(log_expr) %in% housekeeping_genes
  if (sum(controls) < 2) {
    nacho_abort(
      c(
        "RUVg needs at least two housekeeping genes as controls.",
        i = "Set {.arg housekeeping_genes}, or use {.code normalisation_method = \"GEO\"}."
      ),
      class = "bad_argument",
      call = call
    )
  }
  list(log_expr = log_expr, controls = controls, rows = rows)
}

#' Rebuild the scaled counts of an object, then its RUVg input
#'
#' @noRd
ruv_input <- function(
  counts,
  probes,
  samples,
  settings,
  housekeeping_genes,
  call = rlang::caller_env()
) {
  scaled <- scale_counts(
    counts,
    if (all(is.na(samples[["Background"]]))) NULL else samples[["Background"]],
    settings[["background_mode"]],
    samples[["Positive_factor"]]
  )
  ruv_input_from_scaled(scaled, probes, housekeeping_genes, call = call)
}

#' The names of the housekeeping genes of an object
#'
#' @noRd
housekeeping_names <- function(x) {
  x@probes[["Name"]][x@probes[["is_housekeeping"]]]
}

#' RLE and PCA diagnostic for each k
#'
#' @noRd
ruv_k_table <- function(log_expr, controls, max_k) {
  max_k <- min(max_k, sum(controls) - 1L, nrow(log_expr) - 1L)
  table <- data.frame(k = 0:max(max_k, 0L))
  corrected <- lapply(table[["k"]], function(k) {
    ruvg(log_expr, controls, k)[["corrected"]]
  })
  table[["rle_iqr"]] <- vapply(corrected, rle_iqr, numeric(1))
  table[["pc1_variance"]] <- vapply(
    corrected,
    function(y) {
      d <- svd(scale(
        y[, colSums(is.na(y)) == 0, drop = FALSE],
        scale = FALSE
      ))[["d"]]
      d[1]^2 / sum(d^2)
    },
    numeric(1)
  )
  best <- min(table[["rle_iqr"]])
  table[["suggested"]] <- table[["k"]] ==
    min(table[["k"]][table[["rle_iqr"]] <= 1.05 * best])
  table
}

#' Suggest how many unwanted factors RUVg should remove
#'
#' For each `k` from 0 to `max_k`, corrects the log2 positive-normalised
#' counts with RUVg, using the housekeeping genes as controls, and reports the
#' mean interquartile range of the relative log expression (RLE) and the share
#' of variance on the first principal component.
#' The suggested `k` is the smallest whose RLE spread is within 5 % of the
#' smallest one: removing more factors risks removing biology.
#'
#' @param x A `nacho` object from [load_rcc()] or [normalise()].
#' @param max_k The largest `k` to try; it is lowered to one less than the
#'   number of housekeeping genes or of samples when those are smaller.
#'
#' @return A data frame with `k`, `rle_iqr`, `pc1_variance` and `suggested`.
#' @export
#' @examples
#' data(GSE74821)
#' suggest_ruv_k(GSE74821)
suggest_ruv_k <- function(x, max_k = 5) {
  check_nacho(x)
  check_count(max_k, min = 0)
  input <- ruv_input(
    x@counts,
    x@probes,
    x@samples,
    x@settings,
    housekeeping_names(x)
  )
  ruv_k_table(input[["log_expr"]], input[["controls"]], as.integer(max_k))
}
