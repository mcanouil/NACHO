#' geometric_housekeeping
#'
#' @param data [data.frame]
#' @param positive_factor [vector(numeric)]
#' @param intercept [vector(numeric)]
#' @param housekeeping_genes [vector(character)]
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
