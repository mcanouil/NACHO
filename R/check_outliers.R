#' check_outliers
#'
#' @inheritParams normalise
#'
#' @keywords internal
#'
#' @return [[list]] A list containing parameters and data.#'
check_outliers <- function(nacho_object) {
  ot <- nacho_object[["outliers_thresholds"]]
  nacho_object[["nacho"]][, "is_outlier"] <- {
    nacho_object[["nacho"]][, "BD"] < min(ot[["BD"]]) | nacho_object[["nacho"]][, "BD"] > max(ot[["BD"]]) |
    nacho_object[["nacho"]][, "FoV"] < ot[["FoV"]] |
    nacho_object[["nacho"]][, "PCL"] < ot[["PCL"]] |
    nacho_object[["nacho"]][, "LoD"] < ot[["LoD"]] |
    nacho_object[["nacho"]][, "Positive_factor"] < min(ot[["Positive_factor"]]) |
      nacho_object[["nacho"]][, "Positive_factor"] > max(ot[["Positive_factor"]]) |
    nacho_object[["nacho"]][, "House_factor"] < min(ot[["House_factor"]]) |
      nacho_object[["nacho"]][, "House_factor"] > max(ot[["House_factor"]])
  }
  nacho_object
}