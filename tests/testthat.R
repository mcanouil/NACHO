library(testthat)
library(NACHO)

# setwd("tests")

rcc_files_directory <- "testthat/plexset_data"
plexset_tidy <- data.frame(stringsAsFactors = FALSE,
  name = list.files(rcc_files_directory),
  datapath = list.files(rcc_files_directory, full.names = TRUE),
  IDFILE = basename(list.files(rcc_files_directory, full.names = TRUE)),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(list.files(rcc_files_directory)))
)
plexset_nacho <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = plexset_tidy,
  id_colname = "IDFILE"
)

rcc_files_directory <- "testthat/salmon_data"
salmon_tidy <- data.frame(stringsAsFactors = FALSE,
  name = list.files(rcc_files_directory),
  datapath = list.files(rcc_files_directory, full.names = TRUE),
  IDFILE = basename(list.files(rcc_files_directory, full.names = TRUE)),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(list.files(rcc_files_directory)))
)
salmon_nacho <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = salmon_tidy,
  id_colname = "IDFILE"
)

test_check("NACHO")
