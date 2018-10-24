#' qc_positive_control
#'
#' @param counts
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom stats cor.test
qc_positive_control <- function(counts) {
  measured <- log2(counts[["Count"]])
  known <- log2(as.numeric(gsub("^[^(]*\\((.*)\\)$", "\\1", counts[["Name"]]))) # plexset value: "32"
  correlation <- stats::cor.test(known, measured)$estimate
  return(unname(round(correlation, 5)))
}