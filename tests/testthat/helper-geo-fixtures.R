geo_fixture <- function(series) {
  dir <- system.file("extdata", series, package = "NACHO")
  list(
    dir = dir,
    samplesheet = utils::read.csv(file.path(dir, "samplesheet.csv"))
  )
}

first_fixture_file <- function(series) {
  list.files(
    geo_fixture(series)$dir,
    pattern = "\\.RCC\\.gz$",
    full.names = TRUE
  )[1]
}

mirna_fixture <- function(...) {
  fixture <- geo_fixture("GSE270837")
  suppressMessages(NACHO::load_rcc(
    fixture[["dir"]],
    fixture[["samplesheet"]],
    "IDFILE",
    instrument = "sprint",
    n_comp = 3,
    ...
  ))
}
