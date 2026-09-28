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
