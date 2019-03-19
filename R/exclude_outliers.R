#' exclude_outliers
#'
#' @inheritParams normalise
#'
#' @keywords internal
#'
#' @return [data.frame]
exclude_outliers <- function(nacho_object) {
  nacho_df <- nacho_object[["nacho"]]
  id_colname <- nacho_object[["access"]]
  all_out <- details_outlier(
    nacho_df = nacho_df,
    id_colname = id_colname,
    outliers_thresholds = nacho_object[["outliers_thresholds"]]
  )
  if (!nacho_object[["housekeeping_norm"]]) {
    all_out <- all_out[!grepl("house_out", names(all_out))]
  }
  all_out <- unique(unlist(all_out))
  if (length(all_out)!=0) {
    output <- nacho_df[!nacho_df[[id_colname]] %in% all_out, ]
  } else {
    output <- nacho_df
  }

  output
}
