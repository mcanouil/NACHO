#' qc_positive_control
#'
#' @param counts [[data.frame]] A `data.frame` with the count data.
#'
#' @keywords internal
#' @usage NULL
#'
#' @return [[numeric]]
qc_positive_control <- function(counts) {
  measured <- log2(counts[["Count"]])
  known <- log2(as.numeric(gsub("^[^(]*\\((.*)\\)$", "\\1", counts[["Name"]]))) # plexset value: "32"
  correlation <- stats::cor.test(known, measured)$estimate
  unname(round(correlation, 5))
}