test_that("nacho_counts() returns raw, normalised and log2 matrices", {
  x <- toy_nacho()
  expect_identical(nacho_counts(x), x@counts)
  expect_identical(nacho_counts(x, normalised = TRUE), x@normalised)
  expect_equal(nacho_counts(x, log2 = TRUE), log2(x@counts + 1))
  expect_error(nacho_counts(x, log2 = NA), class = "nacho_error_bad_argument")
})

test_that("nacho_samples() puts the id first and appends the PCA scores", {
  samples <- nacho_samples(toy_nacho())
  expect_identical(names(samples)[1], "IDFILE")
  expect_true(all(c("PC01", "PC02") %in% names(samples)))
  expect_identical(nrow(samples), 4L)
})

test_that("nacho_samples() works when the PCA has zero components", {
  x <- toy_nacho()
  x@pca <- list(
    scores = x@pca$scores[, 0, drop = FALSE],
    importance = x@pca$importance[0, ]
  )
  samples <- nacho_samples(x)
  expect_identical(samples, x@samples)
  expect_false(any(c("PC01", "PC02") %in% names(samples)))
})

test_that("nacho_probes() and nacho_qc() return one row per probe and per sample", {
  x <- toy_nacho()
  expect_identical(nrow(nacho_probes(x)), 11L)
  qc <- nacho_qc(x)
  expect_identical(nrow(qc), 4L)
  expect_true(all(
    c("IDFILE", "BD", "FoV", "PCL", "LoD", "is_outlier") %in% names(qc)
  ))
})

test_that("accessors refuse other objects", {
  expect_error(nacho_counts(list()), class = "nacho_error_bad_object")
  v2 <- structure(list(nacho = data.frame()), class = "nacho")
  expect_error(nacho_samples(v2), class = "nacho_error_bad_object")
  expect_snapshot(nacho_samples(v2), error = TRUE)
})
