#' geometric_probes
#'
#' @param data [list(data.frame)]
#' @param probes_type [character]
#' @param exclude_probes [vector(character)]
#'
#' @return [numeric]
geometric_probes <- function(data, probes_type) {
  sapply(
    X = data,
    FUN = function(.data) {
      .data <- .data[.data[["CodeClass"]] %in% probes_type, ]
      .data[.data == 0] <- 1
      geometric_mean(.data[["Count"]])
    }
  )
}