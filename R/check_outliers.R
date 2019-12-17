#' Annotate a "nacho" object for outliers
#'
#' Add or update `"is_outlier"` column in the `"nacho"` field of an object from
#' a call to `summarise()` or `normalise()`, using the current quality-control thresholds.
#'
#' @inheritParams normalise
#'
#' @export
#'
#' @return [[list]] A list containing parameters and data.
#'
#' @examples
#'
#' data(GSE74821)
#' nacho_object <- check_outliers(GSE74821)
#' head(nacho_object$nacho)
#'
check_outliers <- function(nacho_object) {
  if (!attr(nacho_object, "RCC_type") %in% c("n1", "n8")) {
    stop('[NACHO] RCC type must be either "n1" or "n8"!')
  }
  ot <- nacho_object[["outliers_thresholds"]]
  if (attr(nacho_object, "RCC_type") == "n1") {
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
  } else {
    nacho_object[["nacho"]][, "is_outlier"] <- {
      nacho_object[["nacho"]][, "BD"] < min(ot[["BD"]]) | nacho_object[["nacho"]][, "BD"] > max(ot[["BD"]]) |
      nacho_object[["nacho"]][, "FoV"] < ot[["FoV"]] |
      nacho_object[["nacho"]][, "Positive_factor"] < min(ot[["Positive_factor"]]) |
        nacho_object[["nacho"]][, "Positive_factor"] > max(ot[["Positive_factor"]]) |
      nacho_object[["nacho"]][, "House_factor"] < min(ot[["House_factor"]]) |
        nacho_object[["nacho"]][, "House_factor"] > max(ot[["House_factor"]])
    }
  }
  nacho_object
}