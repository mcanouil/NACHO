#' exclude_outliers
#'
#' @param object [nacho_set]
#'
#' @return [data.frame]
exclude_outliers <- function(object) {
  nacho_df <- object["nacho"]
  id_colname <- object["access"]
  all_out <- details_outlier(nacho_df = nacho_df, id_colname = id_colname)
  if (!object["housekeeping_norm"]) {
    all_out <- all_out[!grepl("house_out", names(all_out))]
  }
  all_out <- unique(unlist(all_out))
  if (length(all_out)!=0) {
    return(nacho_df[!nacho_df[[id_colname]] %in% all_out, ])
  } else {
    return(nacho_df)
  }
}
