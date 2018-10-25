require(shiny)
require(ggplot2)
require(scales)
require(ggbeeswarm)
require(ggrepel)
require(dplyr)
require(tidyr)
require(ggpubr)
require(tibble)
require(NACHO)

# load(file = paste0(tempdir(), "/nacho_shiny.Rdata"))

font_size <- 14
ggplot2::theme_set(ggplot2::theme_light(base_size = font_size))


id_colname <- nacho_shiny["access"]
housekeeping_genes <- nacho_shiny["housekeeping_genes"]
pc_sum <- nacho_shiny["pc_sum"]
nacho <- nacho_shiny["nacho"]
save_path <- nacho_shiny["data_directory"]
