#' geometric_housekeeping
#'
#' @param data
#' @param positive_factor
#' @param intercept
#' @param housekeeping_genes
#'
#' @return
#' @export
#'
#' @examples
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
  return(geometric_mean(house_data[["Count"]]))
}
