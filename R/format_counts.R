#' format_counts
#'
#' @param data [[data.frame]] A `data.frame` with the count data.
#' @inheritParams load_rcc
#' @param count_column [[character]] A `character` string naming the column where the counts are.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[data.frame]]
format_counts <- function(data, id_colname, count_column = "Count") {
  data.table::dcast(
    data = data.table::setDT(data),
    formula = sprintf("CodeClass + Name ~ %s", id_colname),
    value.var = count_column
  )
}
