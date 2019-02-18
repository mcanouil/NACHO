#' normalise_counts
#'
#' @param data [data.frame]
#' @param housekeeping_norm [logical]
#'
#' @return [vector(numeric)]
normalise_counts <- function(data, housekeeping_norm) {
  out <- (data[["Count"]] - data[["Negative_factor"]]) *
    data[["Positive_factor"]]
  if (housekeeping_norm) {
    out <- out * data[["House_factor"]]
  }
  out[out<=0] <- 0.1
  return(round(out))
}
