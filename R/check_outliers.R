#' Annotate a "nacho" object for outliers
#'
#' Add or update `"is_outlier"` column in the `"nacho"` field of an object from
#' a call to [`load_rcc()`] or [`normalise()`] (`nacho_object$nacho`),
#' using the current quality-control thresholds.
#'
#' @inheritParams normalise
#'
#' @export
#'
#' @return A [[list]] object of class `"nacho"`.
#'
#' @examples
#'
#' data(GSE74821)
#' nacho_object <- check_outliers(GSE74821)
#' head(nacho_object$nacho)
#'
check_outliers <- function(nacho_object) {
  if (missing(nacho_object)) {
    stop(
      '[NACHO] "nacho_object" is missing, results from "load_rcc()" and/or "normalise()" is mandatory!'
    )
  }
  if (!attr(nacho_object, "RCC_type") %in% c("n1", "n8")) {
    stop('[NACHO] RCC type must be either "n1" or "n8"!')
  }

  ot <- nacho_object[["outliers_thresholds"]]
  nacho_df <- nacho_object[["nacho"]]

  outside <- function(metric, limits) {
    values <- nacho_df[[metric]]
    !is.na(values) & (values < min(limits) | values > max(limits))
  }
  below <- function(metric, limit) {
    values <- nacho_df[[metric]]
    !is.na(values) & values < limit
  }

  is_outlier <- outside("BD", ot[["BD"]]) |
    below("FoV", ot[["FoV"]]) |
    outside("Positive_factor", ot[["Positive_factor"]])
  if ("House_factor" %in% colnames(nacho_df)) {
    is_outlier <- is_outlier | outside("House_factor", ot[["House_factor"]])
  }
  if (attr(nacho_object, "RCC_type") == "n1") {
    is_outlier <- is_outlier |
      below("PCL", ot[["PCL"]]) |
      below("LoD", ot[["LoD"]])
  }

  nacho_object[["nacho"]][["is_outlier"]] <- is_outlier
  nacho_object
}
