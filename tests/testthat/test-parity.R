parity <- readRDS(test_path("fixtures", "parity-1a.rds"))

nacho_2_rounding <- function(m) {
  m <- round(m)
  m[!is.na(m) & m <= 0] <- 0.1
  m
}

glm_metrics <- c("MC", "MedC", "Positive_factor", "PCL", "LoD", "BD", "FoV")

check_parity <- function(x, reference, glm = FALSE) {
  testthat::expect_identical(x@counts, reference[["counts"]])
  if (!glm) {
    testthat::expect_equal(
      nacho_2_rounding(x@normalised),
      reference[["normalised"]],
      tolerance = 1e-8
    )
  }
  has_flags <- "is_outlier" %in% names(reference[["metrics"]])
  columns <- if (glm) {
    glm_metrics
  } else {
    setdiff(names(reference[["metrics"]]), "is_outlier")
  }
  testthat::expect_equal(
    x@samples[, columns],
    reference[["metrics"]][, columns],
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  if (has_flags) {
    testthat::expect_identical(
      nacho_qc(x)$status %in% "fail",
      reference[["metrics"]][["is_outlier"]] %in% TRUE
    )
  }
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
  plexset_geo <- suppressMessages(load_rcc(
    data_directory = test_path("plexset_data"),
    ssheet_csv = plexset_tidy,
    id_colname = "IDFILE",
    housekeeping_norm = FALSE,
    background = "geo",
    background_mode = "subtract",
    preset = "legacy"
  ))
  check_parity(plexset_geo, parity[["plexset"]])
})

test_that("single-sample results match the NACHO 2 pipeline", {
  fixture <- geo_fixture("GSE178516")
  load <- function(...) {
    suppressWarnings(suppressMessages(NACHO::load_rcc(
      fixture[["dir"]],
      fixture[["samplesheet"]],
      "IDFILE",
      n_comp = 5,
      background = "geo",
      background_mode = "subtract",
      preset = "legacy",
      ...
    )))
  }
  check_parity(load(), parity[["io360_geo"]])
  check_parity(
    load(normalisation_method = "GLM"),
    parity[["io360_glm"]],
    glm = TRUE
  )
  check_parity(load(housekeeping_predict = TRUE), parity[["io360_predict"]])
})

test_that("miRNA results match the NACHO 2 pipeline", {
  fixture <- geo_fixture("GSE270837")
  x <- suppressWarnings(suppressMessages(load_rcc(
    fixture[["dir"]],
    fixture[["samplesheet"]],
    "IDFILE",
    n_comp = 5,
    background = "geo",
    background_mode = "subtract",
    preset = "legacy"
  )))
  check_parity(x, parity[["mirna"]])
})

test_that("salmon results match the NACHO 2 pipeline", {
  salmon_geo <- suppressMessages(load_rcc(
    data_directory = test_path("salmon_data"),
    ssheet_csv = salmon_tidy,
    id_colname = "IDFILE",
    background = "geo",
    background_mode = "subtract",
    preset = "legacy"
  ))
  check_parity(salmon_geo, parity[["salmon"]])
})
