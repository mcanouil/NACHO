#' find_housekeeping
#'
#' @param data [[data.frame]] A `data.frame` with the count data.
#' @inheritParams load_rcc
#' @param count_column [[character]] A `character` string naming the column where the counts are.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[character]]
find_housekeeping <- function(data, id_colname, count_column) {
  CodeClass <- NULL # no visible binding for global variable
  data <- data[grep("Endogenous|Housekeeping", CodeClass)]
  ratios <- lapply(
    X = split(data, data[[id_colname]]),
    count_column = count_column,
    FUN = function(.data, count_column) {
      .data <- .data[j = .SD, .SDcols = c("Name", count_column)]
      sample_means <- mean(.data[[count_column]])
      ratios <- log2(.data[[count_column]] / sample_means)
      names(ratios) <- .data[["Name"]]
      ratios[is.infinite(ratios)] <- NA_real_
      ratios
    }
  )
  ratios <- do.call("cbind", ratios)
  ratios_sd <- apply(X = ratios, MARGIN = 1, FUN = stats::sd, na.rm = TRUE)
  ratios_sd <- sort(ratios_sd, decreasing = FALSE)
  names(ratios_sd)[1:5]
}
