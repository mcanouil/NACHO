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
  alpha <- solve(crossprod(w), crossprod(w, log_expr))
  list(W = w, corrected = log_expr - w %*% alpha)
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
