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
  suppressMessages(normalise(GSE74821, normalisation_method = "GEO"))
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

test_that("a successful GLM matches the NACHO 2 slope on real data", {
  x <- suppressMessages(normalise(geo_object(), normalisation_method = "GLM"))
  expect_null(x@provenance[["glm_fallback"]])
  expect_identical(x@settings[["normalisation_method"]], "GLM")
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
  fallen@counts <- GSE74821@counts
  refit <- suppressMessages(normalise(fallen, normalisation_method = "GLM"))
  expect_null(refit@provenance[["glm_fallback"]])
})
