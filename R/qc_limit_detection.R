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
  negatives_sd <- stats::sd(negatives)
  if (is.na(negatives_sd) || negatives_sd == 0) {
    return(NA_real_)
  }
  z_score <- (pos_e - mean(negatives)) / negatives_sd
  round(z_score, 2)
}
