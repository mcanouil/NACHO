require(shiny)
require(ggplot2)
require(scales)
require(ggbeeswarm)
require(ggrepel)
require(NACHO)

load(file = paste0(tempdir(), "/nacho_shiny.Rdata"))

font_size <- as.numeric(nacho_shiny[["font_size"]])
ggplot2::theme_set(ggplot2::theme_light(base_size = font_size))


id_colname <- nacho_shiny[["access"]]
housekeeping_genes <- nacho_shiny[["housekeeping_genes"]]
pc_sum <- nacho_shiny[["pc_sum"]]
nacho <- nacho_shiny[["nacho"]]
save_path <- nacho_shiny[["data_directory"]]


# details_outlier <- function(nacho_df, id_colname) {
#   binding_out <- nacho_df[which(nacho_df[["BD"]] > 2.25 | nacho_df[["BD"]] < 0.1), id_colname]
#   fov_out <- nacho_df[which(nacho_df[["FoV"]] < 75), id_colname]
#   if (!all(nacho_df[["PC"]]==0)) {
#     pc_out <- nacho_df[which(nacho_df[["PC"]] < 0.95), id_colname]
#   } else {
#     pc_out <- NULL
#   }
#   if (!all(nacho_df[["LoD"]]==0)) {
#     lod_out <- nacho_df[which(nacho_df[["LoD"]] < 2), id_colname]
#   } else {
#     lod_out <- NULL
#   }
#   fac_out <- nacho_df[which(nacho_df[["Positive_factor"]]<(1/4) | nacho_df[["Positive_factor "]]>4), id_colname]
#   house_out <- nacho_df[which(nacho_df[["House_factor"]]<(1/11) | nacho_df[["House_factor"]]>11), id_colname]
#
#   all_out <- list(
#     "binding_out" = unique(binding_out),
#     "fov_out" = unique(fov_out),
#     "pc_out" = unique(pc_out),
#     "lod_out" = unique(lod_out),
#     "house_out" = unique(house_out),
#     "fac_out" = unique(fac_out)
#   )
#
#   return(all_out)
# }
# normalise_counts <- function(data) {
#   out <- (data[["Count"]] - data[["Negative_factor"]]) *
#     data[["Positive_factor"]] *
#     data[["House_factor"]]
#   out[out<=0] <- 0.1
#   return(round(out))
# }