library(testthat)
library(NACHO)

# setwd("tests")

rcc_files_directory <- "testthat/plexset_data"
plexset_files <- list.files(rcc_files_directory, full.names = TRUE, pattern = "\\.RCC")
plexset_tidy <- data.frame(stringsAsFactors = FALSE,
  name = basename(plexset_files),
  datapath = plexset_files,
  IDFILE = basename(plexset_files),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(plexset_files))
)
plexset_nacho <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = plexset_tidy,
  id_colname = "IDFILE"
)

rcc_files_directory <- "testthat/salmon_data"
salmon_files <- list.files(rcc_files_directory, full.names = TRUE, pattern = "\\.RCC")
salmon_tidy <- data.frame(stringsAsFactors = FALSE,
  name = basename(plexset_files),
  datapath = plexset_files,
  IDFILE = basename(plexset_files),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(plexset_files))
)
salmon_nacho <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = salmon_tidy,
  id_colname = "IDFILE"
)

rcc_files_directory <- "testthat"
plexset_salmon_tidy <- data.frame(stringsAsFactors = FALSE,
  name = list.files(rcc_files_directory, pattern = "\\.RCC", recursive = TRUE),
  datapath = list.files(rcc_files_directory, full.names = TRUE, pattern = "\\.RCC", recursive = TRUE),
  IDFILE = basename(list.files(rcc_files_directory, full.names = TRUE, pattern = "\\.RCC", recursive = TRUE)),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(list.files(rcc_files_directory, pattern = "\\.RCC", recursive = TRUE)))
)

test_check("NACHO")
