test_that("the GLM recovers an exact line", {
  concentration <- c(0, 0, 0.5, 2, 8, 32, 128)
  counts <- 10 + 100 * concentration
  expect_equal(NACHO:::glm_slope(concentration, counts), 100, tolerance = 1e-6)
})

test_that("a fit that cannot be a positive line gives NA", {
  concentration <- c(0, 0, 0.5, 2, 8, 32, 128)
  expect_identical(
    NACHO:::glm_slope(concentration, rev(10 + 100 * concentration)),
    NA_real_
  )
})

geo_object <- function() {
  suppressMessages(normalise(NACHO::GSE74821, normalisation_method = "GEO"))
}

test_that("a failed GLM warns, names the samples and falls back to GEO", {
  x <- geo_object()
  positives <- nacho_probes(x)[["CodeClass"]] == "Positive"
  x@counts[positives, 1] <- rev(x@counts[positives, 1])
  expect_warning(
    y <- suppressMessages(normalise(x, normalisation_method = "GLM")),
    regexp = colnames(x@counts)[1],
    class = "nacho_warning_glm_convergence"
  )
  expect_identical(y@settings[["normalisation_method"]], "GEO")
  expect_identical(y@provenance[["glm_fallback"]], colnames(x@counts)[1])
  probes <- nacho_probes(y)
  expected <- NACHO:::control_factors(
    x@counts,
    probes,
    probes[["Name"]][probes[["is_excluded"]]],
    "GEO"
  )[["positive_factor"]]
  expect_equal(nacho_samples(y)[["Positive_factor"]], unname(expected))
})

test_that("a successful GLM keeps the method and records no fallback", {
  x <- suppressMessages(normalise(geo_object(), normalisation_method = "GLM"))
  expect_null(x@provenance[["glm_fallback"]])
  expect_identical(x@settings[["normalisation_method"]], "GLM")
  probes <- nacho_probes(x)
  names <- probes[["Name"]]
  used <- probes[["CodeClass"]] %in%
    c("Positive", "Negative") &
    !names %in% c("POS_F(0.125)", names[probes[["is_excluded"]]])
  concentration <- NACHO:::control_concentrations(names[used])
  slopes <- apply(
    NACHO::GSE74821@counts[used, , drop = FALSE],
    2,
    function(counts) NACHO:::glm_slope(concentration, counts)
  )
  expect_equal(
    nacho_samples(x)[["Positive_factor"]],
    unname(mean(slopes) / slopes)
  )
})

test_that("the fallback record is cleared by the next build", {
  x <- geo_object()
  positives <- nacho_probes(x)[["CodeClass"]] == "Positive"
  x@counts[positives, 1] <- rev(x@counts[positives, 1])
  fallen <- suppressWarnings(
    suppressMessages(normalise(x, normalisation_method = "GLM"))
  )
  expect_false(is.null(fallen@provenance[["glm_fallback"]]))
  again <- suppressMessages(
    normalise(fallen, housekeeping_norm = FALSE)
  )
  expect_null(again@provenance[["glm_fallback"]])
  fallen@counts <- NACHO::GSE74821@counts
  refit <- suppressMessages(normalise(fallen, normalisation_method = "GLM"))
  expect_null(refit@provenance[["glm_fallback"]])
})

test_that("several failed samples are listed with a truncation", {
  x <- geo_object()
  positives <- nacho_probes(x)[["CodeClass"]] == "Positive"
  for (k in 1:7) {
    x@counts[positives, k] <- rev(x@counts[positives, k])
  }
  expect_warning(
    y <- suppressMessages(normalise(x, normalisation_method = "GLM")),
    regexp = "\\.\\.\\.",
    class = "nacho_warning_glm_convergence"
  )
  expect_identical(y@provenance[["glm_fallback"]], colnames(x@counts)[1:7])
})

test_that("the fallback record survives a save and read round trip", {
  x <- geo_object()
  positives <- nacho_probes(x)[["CodeClass"]] == "Positive"
  x@counts[positives, 1] <- rev(x@counts[positives, 1])
  fallen <- suppressWarnings(
    suppressMessages(normalise(x, normalisation_method = "GLM"))
  )
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(fallen, path)
  restored <- suppressMessages(read_nacho(path))
  expect_identical(
    restored@provenance[["glm_fallback"]],
    fallen@provenance[["glm_fallback"]]
  )
  expect_false(is.null(restored@provenance[["glm_fallback"]]))
})

test_that("a sample with fewer than two kept controls gives NA", {
  concentration <- c(0, 0, 0.5, 2, 8, 32, 128)
  expect_identical(
    NACHO:::glm_slope(concentration, rep(NA_real_, 7)),
    NA_real_
  )
  expect_identical(
    NACHO:::glm_slope(concentration, c(5, rep(NA_real_, 6))),
    NA_real_
  )
})

test_that("a missing concentration cannot abort the GLM fit", {
  concentration <- c(NA, 0, 0.5, 2, 8, 32, 128)
  expect_identical(
    NACHO:::glm_slope(concentration, c(5, rep(NA_real_, 6))),
    NA_real_
  )
  expect_identical(
    NACHO:::glm_slope(concentration, c(5, 3, rep(NA_real_, 5))),
    NA_real_
  )
})

test_that("print() says when GLM fell back to the geometric mean", {
  x <- geo_object()
  positives <- nacho_probes(x)[["CodeClass"]] == "Positive"
  x@counts[positives, 1] <- rev(x@counts[positives, 1])
  fallen <- suppressWarnings(
    suppressMessages(normalise(x, normalisation_method = "GLM"))
  )
  expect_match(
    format(fallen),
    "GLM fell back to the geometric mean for 1 sample",
    all = FALSE
  )
  expect_false(any(grepl("GLM fell back", format(x))))
})
