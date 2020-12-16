#' probe_exclusion
#'
#' @param control_genes_df [[data.frame]] A `data.frame` with the count data.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[character]]
probe_exclusion <- function(control_genes_df) {
  count_columns <- sapply(X = control_genes_df, FUN = is.numeric)
  local_neg <- control_genes_df[control_genes_df[["CodeClass"]] %in% "Negative", count_columns]
  overal_median <- stats::median(as.matrix(local_neg))
  medians <- apply(X = local_neg, MARGIN = 1, FUN = stats::median)
  delta_medians <- sapply(medians, function(x) {abs((overal_median - x))})
  ex_probes <- delta_medians[delta_medians > (0.5 * overal_median)]
  if (length(ex_probes) != 0 && nrow(local_neg) != length(ex_probes)) {
    names(ex_probes)
  } else {
    NULL
  }
}
