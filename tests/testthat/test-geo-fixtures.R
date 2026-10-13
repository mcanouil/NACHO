test_that("the GSE270837 miRNA fixture loads offline from gzipped RCC files", {
  fixture <- geo_fixture("GSE270837")
  expect_length(list.files(fixture[["dir"]], pattern = "\\.RCC\\.gz$"), 6)
  expect_no_error(suppressMessages(NACHO::load_rcc(
    data_directory = fixture[["dir"]],
    ssheet_csv = fixture[["samplesheet"]],
    id_colname = "IDFILE",
    n_comp = 5,
    instrument = "sprint"
  )))
})

test_that("the GSE178516 IO 360 fixture loads offline from gzipped RCC files", {
  fixture <- geo_fixture("GSE178516")
  expect_length(list.files(fixture[["dir"]], pattern = "\\.RCC\\.gz$"), 6)
  expect_no_error(suppressMessages(NACHO::load_rcc(
    data_directory = fixture[["dir"]],
    ssheet_csv = fixture[["samplesheet"]],
    id_colname = "IDFILE",
    n_comp = 5
  )))
})

test_that("the GSE346526 nSolver 4.0.1.8 fixture loads offline with plausible QC", {
  fixture <- geo_fixture("GSE346526")
  expect_length(list.files(fixture[["dir"]], pattern = "\\.RCC\\.gz$"), 6)
  x <- expect_no_warning(suppressMessages(NACHO::load_rcc(
    data_directory = fixture[["dir"]],
    ssheet_csv = fixture[["samplesheet"]],
    id_colname = "IDFILE",
    n_comp = 3
  )))
  samples <- nacho_samples(x)
  expect_identical(unique(samples[["Header.header_FileVersion"]]), "1.7")
  expect_identical(
    unique(samples[["Header.header_SoftwareVersion"]]),
    "4.0.1.8"
  )
  expect_identical(x@settings[["panel"]], "mrna")
  expect_false(anyNA(nacho_counts(x, normalised = TRUE)))
  expect_true(all(is.finite(samples[["Positive_factor"]])))
  expect_true(all(is.finite(samples[["PCL"]])))
})

test_that("the fixtures are single-sample RCC files, not PlexSet", {
  for (series in c("GSE270837", "GSE178516", "GSE346526")) {
    files <- list.files(
      geo_fixture(series)[["dir"]],
      pattern = "\\.RCC\\.gz$",
      full.names = TRUE
    )
    expect_false(any(vapply(files, NACHO:::is_plexset_rcc, logical(1))))
  }
})

test_that("the full GSE270837 series loads from GEO", {
  skip_on_cran()
  skip_if_offline()
  suppressMessages(skip_if_not_installed("GEOquery"))
  expect_identical(
    ncol(load_full_series("GSE270837", instrument = "sprint")),
    22L
  )
})

test_that("the full GSE178516 series loads from GEO", {
  skip_on_cran()
  skip_if_offline()
  suppressMessages(skip_if_not_installed("GEOquery"))
  expect_identical(ncol(load_full_series("GSE178516")), 30L)
})

test_that("the full GSE346526 series loads from GEO", {
  skip_on_cran()
  skip_if_offline()
  suppressMessages(skip_if_not_installed("GEOquery"))
  expect_identical(ncol(load_full_series("GSE346526")), 16L)
})
