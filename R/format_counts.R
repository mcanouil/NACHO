#' format_counts
#'
#' @param data [data.frame] A \code{data.frame} with the count data.
#' @inheritParams summarise
#' @param count_column [character] A \code{character} string naming the column where the counts are.
#'
#' @return [data.frame]
format_counts <- function(data, id_colname, count_column = "Count") {
  as.data.frame(
    tidyr::spread(
      data = data[, c(id_colname, "CodeClass", "Name", count_column)],
      key = get(id_colname),
      value = get(count_column)
    )
  )
}
