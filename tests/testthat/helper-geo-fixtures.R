geo_fixture <- function(series) {
  dir <- system.file("extdata", series, package = "NACHO")
  list(
    dir = dir,
    samplesheet = utils::read.csv(file.path(dir, "samplesheet.csv"))
  )
}
