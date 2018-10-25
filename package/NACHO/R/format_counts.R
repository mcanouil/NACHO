#' format_counts
#'
#' @param data [data.frame]
#' @param id_colname [character]
#' @param count_column [character]
#'
#' @return [data.frame]
#'
#' @importFrom tidyr spread
format_counts <- function(data, id_colname, count_column = "Count") {
  counts_df <- tidyr::spread(
    data = data[, c(id_colname, "CodeClass", "Name", count_column)],
    key = get(id_colname),
    value = get(count_column)
  )
  counts_df <- as.data.frame(counts_df)
  return(counts_df)
}
