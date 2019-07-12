#' geometric_housekeeping
#'
#' @param data [data.frame] A `data.frame` with the count data.
#' @param positive_factor [numeric] A `numeric` vector with the positive probe normalisation factor.
#' @param intercept [numeric] A `numeric` vector with the average counts value.
#' @inheritParams summarise
#'
#' @keywords internal
#' @usage NULL
#'
#' @return [numeric]
geometric_housekeeping <- function(data, positive_factor, intercept, housekeeping_genes) {
  house_data <- as.data.frame(data[, c("Name", "CodeClass", "Count")])
  if (is.null(housekeeping_genes)) {
    house_data <- house_data[house_data[["CodeClass"]] %in% "Housekeeping", ]
  } else {
    house_data <- house_data[house_data[["Name"]] %in% housekeeping_genes, ]
  }

  house_data[["Count"]] <- house_data[["Count"]] - intercept
  house_data[["Count"]] <- house_data[["Count"]] * positive_factor
  house_data[["Count"]][house_data[["Count"]] <= 0] <- 1
  geometric_mean(house_data[["Count"]])
}
