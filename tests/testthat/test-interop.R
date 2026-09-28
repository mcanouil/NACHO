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
  expect_identical(nacho_samples(x), nacho_samples(GSE74821))
  expect_identical(nacho_probes(x), nacho_probes(GSE74821))
  expect_identical(x@settings, GSE74821@settings)
  expect_identical(x@rcc_type, GSE74821@rcc_type)
  expect_identical(x@provenance, GSE74821@provenance)
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

test_that("as_nacho() builds from counts and code classes only", {
  skip_if_not_installed("SummarizedExperiment")
  counts <- nacho_counts(GSE74821)
  rownames(counts) <- nacho_probes(GSE74821)[["Name"]]
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts),
    rowData = S4Vectors::DataFrame(
      CodeClass = nacho_probes(GSE74821)[["CodeClass"]]
    )
  )
  expect_warning(
    x <- suppressMessages(as_nacho(se, id_colname = "sample")),
    class = "nacho_warning_metric_unavailable"
  )
  expect_identical(names(nacho_samples(x))[[1]], "sample")
  expect_identical(nacho_samples(x)[["sample"]], colnames(counts))
  expect_identical(nacho_probes(x)[["Name"]], rownames(counts))
})

test_that("as_nacho() uses the default settings when metadata has none", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  S4Vectors::metadata(se)[["nacho"]][["settings"]] <- NULL
  x <- suppressMessages(as_nacho(se))
  expect_identical(x@settings[["id_colname"]], "IDFILE")
  expect_identical(x@thresholds, GSE74821@thresholds)
})

test_that("as_nacho() checks id_colname", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  expect_error(as_nacho(se, id_colname = 1), class = "nacho_error_bad_argument")
})

test_that("as_nacho() blames itself for a bad SummarizedExperiment", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  SummarizedExperiment::rowData(se)$CodeClass <- NULL
  expect_snapshot(as_nacho(se), error = TRUE)
})

test_that("as_nacho() needs unique sample and probe names", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  colnames(se) <- NULL
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
  se <- as_summarized_experiment(GSE74821)
  colnames(se)[2] <- colnames(se)[1]
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
  se <- as_summarized_experiment(GSE74821)
  SummarizedExperiment::rowData(se)$Name <- NULL
  rownames(se) <- NULL
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
  se <- as_summarized_experiment(GSE74821)
  SummarizedExperiment::rowData(se)$Name[2] <- SummarizedExperiment::rowData(
    se
  )$Name[1]
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
})

test_that("as_nacho() needs positive and negative control probes", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  endogenous <- SummarizedExperiment::rowData(se)$CodeClass == "Endogenous"
  expect_error(as_nacho(se[endogenous, ]), class = "nacho_error_bad_object")
  expect_snapshot(as_nacho(se[endogenous, ]), error = TRUE)
})

test_that("as_nacho() needs numeric counts within the integer range", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  counts <- SummarizedExperiment::assay(se, "counts")
  SummarizedExperiment::assay(se, "counts") <- array(
    as.character(counts),
    dim = dim(counts),
    dimnames = dimnames(counts)
  )
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
  se <- as_summarized_experiment(GSE74821)
  big <- SummarizedExperiment::assay(se, "counts") + 0
  big[1, 1] <- 2^32
  SummarizedExperiment::assay(se, "counts") <- big
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
