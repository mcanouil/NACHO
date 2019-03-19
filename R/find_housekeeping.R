#' find_housekeeping
#'
#' @param data [data.frame]
#' @param id_colname [character]
#' @param count_column [character]
#'
#' @return [vector(character)]
find_housekeeping <- function(data, id_colname, count_column) {
  data <- data[unlist(lapply(c("Endogenous", "Housekeeping"), grep, x = data[["CodeClass"]])), ]
  nested_data_df <- tidyr::nest(dplyr::group_by(.data = data, get(id_colname)))
  colnames(nested_data_df)[1] <- id_colname
  ratios <- lapply(
    X = nested_data_df[["data"]],
    count_column = count_column,
    FUN = function(.data, count_column) {
      .data <- .data[, c("Name", count_column)]
      sample_means <- mean(.data[[count_column]])
      ratios <- log2(.data[[count_column]] / sample_means)
      names(ratios) <- .data[["Name"]]
      ratios
    }
  )
  ratios <- do.call("cbind", ratios)
  ratios_sd <- apply(X = ratios, MARGIN = 1, FUN = stats::sd, na.rm = TRUE)
  ratios_sd <- sort(ratios_sd, decreasing = FALSE)
  utils::head(names(ratios_sd), 5)
}
