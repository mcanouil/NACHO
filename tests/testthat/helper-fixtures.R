plexset_files <- list.files(
  test_path("plexset_data"),
  full.names = TRUE,
  pattern = "\\.RCC$"
)
plexset_tidy <- data.frame(
  name = basename(plexset_files),
  datapath = plexset_files,
  IDFILE = basename(plexset_files),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(plexset_files))
)
plexset_nacho <- suppressMessages(load_rcc(
  data_directory = test_path("plexset_data"),
  ssheet_csv = plexset_tidy,
  id_colname = "IDFILE"
))

salmon_files <- list.files(
  test_path("salmon_data"),
  full.names = TRUE,
  pattern = "\\.RCC$"
)
salmon_tidy <- data.frame(
  name = basename(salmon_files),
  datapath = salmon_files,
  IDFILE = basename(salmon_files),
  plexset_id = rep(paste0("S", seq_len(8)), each = length(salmon_files))
)
salmon_nacho <- suppressMessages(load_rcc(
  data_directory = test_path("salmon_data"),
  ssheet_csv = salmon_tidy,
  id_colname = "IDFILE"
))

plexset_salmon_files <- list.files(
  test_path(),
  pattern = "\\.RCC$",
  recursive = TRUE
)
plexset_salmon_tidy <- data.frame(
  name = plexset_salmon_files,
  datapath = file.path(test_path(), plexset_salmon_files),
  IDFILE = basename(plexset_salmon_files),
  plexset_id = rep(
    paste0("S", seq_len(8)),
    each = length(plexset_salmon_files)
  )
)
