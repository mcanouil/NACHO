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
    files <- list.files(
      geo_fixture(series)[["dir"]],
      pattern = "\\.RCC\\.gz$",
      full.names = TRUE
    )
    expect_false(any(vapply(files, NACHO:::is_plexset_rcc, logical(1))))
  }
})

load_full_series <- function(series) {
  download_dir <- withr::local_tempdir(.local_envir = parent.frame())
  GEOquery::getGEOSuppFiles(GEO = series, baseDir = download_dir)
  utils::untar(
    tarfile = file.path(download_dir, series, paste0(series, "_RAW.tar")),
    exdir = file.path(download_dir, series)
  )
  files <- list.files(
    file.path(download_dir, series),
    pattern = "\\.RCC(\\.gz)?$",
    ignore.case = TRUE
  )
  suppressMessages(NACHO::load_rcc(
    data_directory = file.path(download_dir, series),
    ssheet_csv = data.frame(IDFILE = files),
    id_colname = "IDFILE"
  ))
}

test_that("the full GSE270837 series loads from GEO", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("GEOquery")
  expect_no_error(load_full_series("GSE270837"))
})

test_that("the full GSE178516 series loads from GEO", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("GEOquery")
  expect_no_error(load_full_series("GSE178516"))
})
