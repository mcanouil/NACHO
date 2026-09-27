parity <- readRDS(test_path("fixtures", "parity-1a.rds"))

check_parity <- function(x, reference) {
  testthat::expect_identical(x@counts, reference[["counts"]])
  testthat::expect_equal(
    x@normalised,
    reference[["normalised"]],
    tolerance = 1e-8
  )
  columns <- names(reference[["metrics"]])
  testthat::expect_equal(
    x@samples[, columns],
    reference[["metrics"]],
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  testthat::expect_identical(
    sort(x@settings[["housekeeping_genes"]]),
    sort(reference[["housekeeping_genes"]])
  )
  testthat::expect_equal(
    abs(x@pca[["scores"]]),
    reference[["scores"]],
    tolerance = 1e-8
  )
  testthat::expect_equal(
    x@pca[["importance"]],
    reference[["importance"]],
    tolerance = 1e-8
  )
}

test_that("PlexSet results match the NACHO 2 pipeline", {
  check_parity(plexset_nacho, parity[["plexset"]])
})

test_that("single-sample results match the NACHO 2 pipeline", {
  fixture <- geo_fixture("GSE178516")
  load <- function(...) {
    suppressWarnings(suppressMessages(NACHO::load_rcc(
      fixture[["dir"]],
      fixture[["samplesheet"]],
      "IDFILE",
      n_comp = 5,
      ...
    )))
  }
  check_parity(load(), parity[["io360_geo"]])
  check_parity(load(normalisation_method = "GLM"), parity[["io360_glm"]])
  check_parity(load(housekeeping_predict = TRUE), parity[["io360_predict"]])
})

test_that("miRNA results match the NACHO 2 pipeline", {
  fixture <- geo_fixture("GSE270837")
  x <- suppressWarnings(suppressMessages(load_rcc(
    fixture[["dir"]],
    fixture[["samplesheet"]],
    "IDFILE",
    n_comp = 5
  )))
  check_parity(x, parity[["mirna"]])
})

test_that("salmon results match the NACHO 2 pipeline", {
  check_parity(salmon_nacho, parity[["salmon"]])
})
