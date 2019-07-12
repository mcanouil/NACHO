#' geometric_probes
#'
#' @param data [[data.frame]] A `list` of `data.frame` with the count data.
#' @param probes_type [[character]] A `character` string naming the probe type,
#'   *i.e.*, `"Positive"` or `"Negative"`.
#'
#' @keywords internal
#' @usage NULL
#'
#' @return [[numeric]]
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