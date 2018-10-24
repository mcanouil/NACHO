#' geometric_probes
#'
#' @param data
#' @param probes_type
#' @param exclude_probes
#'
#' @return
#' @export
#'
#' @examples
geometric_probes <- function(data, probes_type, exclude_probes = NULL) {
  sapply(
    X = data,
    exclude_probes = exclude_probes,
    FUN = function(.data, exclude_probes) {
      control_data <- as.data.frame(.data[, c("Name", "CodeClass", "Count")])
      control_data <- control_data[control_data[["CodeClass"]] %in% probes_type, ]
      control_data <- control_data[!control_data[["Name"]] %in% exclude_probes, ]
      control_data[control_data == 0] <- 1
      output <- geometric_mean(control_data[["Count"]])
      return(output)
    }
  )
}