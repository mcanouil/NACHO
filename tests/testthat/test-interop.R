test_that("as_summarized_experiment() keeps counts, samples, probes and settings", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  expect_s4_class(se, "SummarizedExperiment")
  expect_identical(
    SummarizedExperiment::assayNames(se),
    c("counts", "normalised")
  )
  expect_identical(
    SummarizedExperiment::assay(se, "counts"),
    nacho_counts(GSE74821)
  )
  expect_identical(colnames(se), nacho_samples(GSE74821)$IDFILE)
  expect_identical(
    S4Vectors::metadata(se)$nacho$settings,
    GSE74821@settings
  )
})

test_that("as_nacho() rebuilds the object from its SummarizedExperiment", {
  skip_if_not_installed("SummarizedExperiment")
  x <- suppressMessages(as_nacho(as_summarized_experiment(GSE74821)))
  expect_identical(nacho_counts(x), nacho_counts(GSE74821))
  expect_equal(
    nacho_counts(x, normalised = TRUE),
    nacho_counts(GSE74821, normalised = TRUE)
  )
  expect_equal(nacho_qc(x), nacho_qc(GSE74821))
  expect_identical(x@thresholds, GSE74821@thresholds)
})

test_that("as_nacho() builds from a plain SummarizedExperiment with default settings", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  S4Vectors::metadata(se) <- list()
  expect_no_warning(x <- suppressMessages(as_nacho(se)))
  expect_identical(x@settings$normalisation_method, "GEO")
  expect_identical(ncol(x), 48L)
})

test_that("as_nacho() needs raw integer counts and code classes", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  SummarizedExperiment::assay(se, "counts") <- SummarizedExperiment::assay(
    se,
    "counts"
  ) +
    0.5
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
  se <- as_summarized_experiment(GSE74821)
  SummarizedExperiment::rowData(se)$CodeClass <- NULL
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
})

test_that("as_nacho() refuses other objects", {
  expect_error(as_nacho(data.frame()), class = "nacho_error_bad_object")
})

test_that("a missing Bioconductor package gives the install command", {
  local_mocked_bindings(has_package = function(package) FALSE)
  expect_error(
    as_summarized_experiment(GSE74821),
    class = "nacho_error_missing_package"
  )
  expect_snapshot(as_summarized_experiment(GSE74821), error = TRUE)
})
