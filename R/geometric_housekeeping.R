#' geometric_housekeeping
#'
#' @param data [[data.frame]] A `data.frame` with the count data.
#' @param positive_factor [[numeric]] A `numeric` vector with the positive probe normalisation factor.
#' @param intercept [[numeric]] A `numeric` vector with the average counts value.
#' @inheritParams load_rcc
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[numeric]]
geometric_housekeeping <- function(data, positive_factor, intercept, housekeeping_genes) {
  Name <- Count <- NULL # no visible binding for global variable
  # if (!is.null(housekeeping_genes)) {
  house_data <- data[
    i = Name %in% housekeeping_genes
  ]
  # } else {
  #   house_data <- data
  # }
  house_data <- house_data[
    j = .SD,
    .SDcols = c("Name", "CodeClass", "Count")
  ][
    j = Count := (Count - intercept) * positive_factor
  ][
    Count <= 0,
    Count := 1
  ]

  geometric_mean(house_data[["Count"]])
}
