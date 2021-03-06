#' qc_limit_detection
#'
#' @param pos_e [[numeric]] Position of Positive probe "POS_E", *i.e.*, a too low expressed probe.
#' @param negatives [[numeric]] A `numeric` vector with the count data from the Negatives probes.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[numeric]]
qc_limit_detection <- function(pos_e, negatives) {
  z_score <- (pos_e - mean(negatives)) / stats::sd(negatives)
  round(z_score, 2)
}