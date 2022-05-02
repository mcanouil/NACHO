library(testthat)
library(NACHO)

rcc_files_directory <- "testthat/plexset_data"
targets <- data.frame(stringsAsFactors = FALSE,
  name = list.files(rcc_files_directory),
  datapath = list.files(rcc_files_directory, full.names = TRUE)
)
targets$IDFILE <- basename(targets$datapath)
targets$plexset_id <- rep(list(paste0("S", seq_len(8))), each = nrow(targets))
plexset_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))

plexset_nacho <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = plexset_tidy,
  id_colname = "IDFILE"
)

rcc_files_directory <- "testthat/salmon_data"
targets <- data.frame(stringsAsFactors = FALSE,
  name = list.files(rcc_files_directory),
  datapath = list.files(rcc_files_directory, full.names = TRUE)
)
targets$IDFILE <- basename(targets$datapath)
targets$plexset_id <- rep(list(paste0("S", seq_len(8))), each = nrow(targets))
salmon_tidy <- as.data.frame(tidyr::unnest(targets, "plexset_id"))
salmon_nacho <- load_rcc(
  data_directory = rcc_files_directory,
  ssheet_csv = salmon_tidy,
  id_colname = "IDFILE"
)

test_check("NACHO")
