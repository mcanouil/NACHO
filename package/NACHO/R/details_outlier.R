#' details_outlier
#'
#' @param nacho_df [data.frame]
#' @param id_colname [character]
#'
#' @return [list]
details_outlier <- function(nacho_df, id_colname) {
  binding_out <- nacho_df[which(nacho_df[["BD"]] > 2.25 | nacho_df[["BD"]] < 0.1), id_colname]
  fov_out <- nacho_df[which(nacho_df[["FoV"]] < 75), id_colname]
  if (!all(nacho_df[["PC"]]==0)) {
    pc_out <- nacho_df[which(nacho_df[["PC"]] < 0.95), id_colname]
  } else {
    pc_out <- NULL
  }
  if (!all(nacho_df[["LoD"]]==0)) {
    lod_out <- nacho_df[which(nacho_df[["LoD"]] < 2), id_colname]
  } else {
    lod_out <- NULL
  }
  fac_out <- nacho_df[which(nacho_df[["Positive_factor"]]<(1/4) | nacho_df[["Positive_factor "]]>4), id_colname]
  house_out <- nacho_df[which(nacho_df[["House_factor"]]<(1/11) | nacho_df[["House_factor"]]>11), id_colname]

  all_out <- list(
    "binding_out" = unique(binding_out),
    "fov_out" = unique(fov_out),
    "pc_out" = unique(pc_out),
    "lod_out" = unique(lod_out),
    "house_out" = unique(house_out),
    "fac_out" = unique(fac_out)
  )

  return(all_out)
}
