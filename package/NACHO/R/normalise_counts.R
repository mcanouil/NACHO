#' normalise_counts
#'
#' @param data [data.frame]
#'
#' @return [vector(numeric)]
normalise_counts <- function(data) {
  out <- (data[["Count"]] - data[["Negative_factor"]]) *
    data[["Positive_factor"]] *
    data[["House_factor"]]
  out[out<=0] <- 0.1
  return(round(out))
}
