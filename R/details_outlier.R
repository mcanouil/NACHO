#' details_outlier
#'
#' @param nacho_df [data.frame]
#' @param id_colname [character]
#' @param outliers_thresholds [list]
#'
#' @return [list]
details_outlier <- function(nacho_df, id_colname, outliers_thresholds) {
  binding_thresh_logical <- which(
    nacho_df[["BD"]] > outliers_thresholds[["BD"]][2] |
      nacho_df[["BD"]] < outliers_thresholds[["BD"]][1]
  )
  binding_out <- nacho_df[[id_colname]][binding_thresh_logical]
  fov_out <- nacho_df[[id_colname]][which(nacho_df[["FoV"]] < outliers_thresholds[["FoV"]])]
  if (!all(nacho_df[["PC"]]==0)) {
    pc_out <- nacho_df[[id_colname]][which(nacho_df[["PC"]] < outliers_thresholds[["PC"]])]
  } else {
    pc_out <- NULL
  }
  if (!all(nacho_df[["LoD"]]==0)) {
    lod_out <- nacho_df[[id_colname]][which(nacho_df[["LoD"]] < outliers_thresholds[["LoD"]])]
  } else {
    lod_out <- NULL
  }
  fac_thresh_logical <- which(
    nacho_df[["Positive_factor"]] < outliers_thresholds[["Positive_factor"]][1] |
      nacho_df[["Positive_factor "]] > outliers_thresholds[["Positive_factor"]][2]
  )
  fac_out <- nacho_df[[id_colname]][fac_thresh_logical]
  house_thresh_logical <- which(
    nacho_df[["House_factor"]] < outliers_thresholds[["House_factor"]][1] |
      nacho_df[["House_factor "]] > outliers_thresholds[["House_factor"]][2]
  )
  house_out <- nacho_df[[id_colname]][house_thresh_logical]

  list(
    "binding_out" = unique(binding_out),
    "fov_out" = unique(fov_out),
    "pc_out" = unique(pc_out),
    "lod_out" = unique(lod_out),
    "house_out" = unique(house_out),
    "fac_out" = unique(fac_out)
  )
}
