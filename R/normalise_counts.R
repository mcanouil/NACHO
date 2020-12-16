#' normalise_counts
#'
#' @param data [[data.frame]] A `data.frame` with the count data.
#' @inheritParams load_rcc
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[numeric]]
normalise_counts <- function(data, housekeeping_norm) {
  out <- (data[["Count"]] - data[["Negative_factor"]]) * data[["Positive_factor"]]
  if (housekeeping_norm & "House_factor" %in% colnames(data)) {
    out <- out * data[["House_factor"]]
  }
  out[out <= 0] <- 0.1
  round(out)
}
