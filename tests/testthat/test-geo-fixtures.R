test_that("the GSE270837 miRNA fixture loads offline from gzipped RCC files", {
  fixture <- geo_fixture("GSE270837")
  expect_length(list.files(fixture[["dir"]], pattern = "\\.RCC\\.gz$"), 6)
  expect_no_error(suppressMessages(NACHO::load_rcc(
    data_directory = fixture[["dir"]],
    ssheet_csv = fixture[["samplesheet"]],
    id_colname = "IDFILE"
  )))
})

test_that("the GSE178516 IO 360 fixture loads offline from gzipped RCC files", {
  fixture <- geo_fixture("GSE178516")
  expect_length(list.files(fixture[["dir"]], pattern = "\\.RCC\\.gz$"), 6)
  expect_no_error(suppressMessages(NACHO::load_rcc(
    data_directory = fixture[["dir"]],
    ssheet_csv = fixture[["samplesheet"]],
    id_colname = "IDFILE"
  )))
})

test_that("the fixtures are single-sample RCC files, not PlexSet", {
  for (series in c("GSE270837", "GSE178516")) {
    files <- list.files(geo_fixture(series)[["dir"]], pattern = "\\.RCC\\.gz$", full.names = TRUE)
    expect_false(any(vapply(files, NACHO:::is_plexset_rcc, logical(1))))
  }
})
