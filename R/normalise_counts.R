#' normalise_counts
#'
#' @param data [data.frame] A \code{data.frame} with the count data.
#' @inheritParams summarise
#'
#' @return [vector(numeric)]
normalise_counts <- function(data, housekeeping_norm) {
  out <- (data[["Count"]] - data[["Negative_factor"]]) * data[["Positive_factor"]]
  if (housekeeping_norm) {
    out <- out * data[["House_factor"]]
  }
  out[out<=0] <- 0.1
  round(out)
}
