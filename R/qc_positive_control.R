#' qc_positive_control
#'
#' @param counts [[data.frame]] A `data.frame` with the count data.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[numeric]]
qc_positive_control <- function(counts) {
  if (any(counts[["Count"]] %in% 0)) {
    measured <- log2(counts[["Count"]] + 1) # nolint: object_usage_linter. Used in lm().
  } else {
    measured <- log2(counts[["Count"]])
  }
  # plexset value: "32"
  known_value <- sub("^[^(]*\\((.*)\\)$", "\\1", counts[["Name"]])
  known <- log2(as.numeric(known_value)) # nolint: object_usage_linter. Used in lm().
  correlation <- summary(stats::lm(measured ~ known))$r.squared
  unname(round(correlation, 5))
}
