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
  expect_equal(nacho_samples(x), nacho_samples(GSE74821))
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

test_that("as_nacho() points NACHO 2 objects to upgrade_nacho()", {
  nacho_2 <- readRDS(test_path("fixtures", "nacho-2-GSE74821-subset.rds"))
  expect_error(as_nacho(nacho_2), class = "nacho_error_bad_object")
  expect_snapshot(as_nacho(nacho_2), error = TRUE)
})

test_that("as_nacho() checks a nacho object and returns it unchanged", {
  expect_identical(as_nacho(GSE74821), GSE74821)
  stale <- GSE74821
  attr(stale, "provenance")$schema_version <- 99L
  expect_error(as_nacho(stale), class = "nacho_error_bad_object")
})

test_that("as_nacho() stamps the current schema on saved provenance", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  S4Vectors::metadata(se)$nacho$provenance$schema_version <- 99L
  x <- suppressMessages(as_nacho(se))
  expect_identical(x@provenance, GSE74821@provenance)
  expect_equal(nacho_samples(x), nacho_samples(GSE74821))
})

se_with_nacho_metadata <- function(field, value) {
  se <- as_summarized_experiment(NACHO::GSE74821)
  nacho_metadata <- S4Vectors::metadata(se)$nacho
  nacho_metadata[[field]] <- value
  S4Vectors::metadata(se)$nacho <- nacho_metadata
  se
}

se_with_setting <- function(setting, value) {
  settings <- NACHO::GSE74821@settings
  settings[setting] <- list(value)
  se_with_nacho_metadata("settings", settings)
}

test_that("as_nacho() checks the settings saved in the metadata", {
  skip_if_not_installed("SummarizedExperiment")
  bad_settings <- list(
    id_colname = 1,
    housekeeping_genes = 1,
    housekeeping_predict = "yes",
    housekeeping_norm = NA,
    normalisation_method = "foo",
    n_comp = 1.5
  )
  for (setting in names(bad_settings)) {
    se <- se_with_setting(setting, bad_settings[[setting]])
    expect_error(as_nacho(se), class = "nacho_error_bad_argument")
  }
  se <- se_with_nacho_metadata("settings", "GEO")
  expect_error(as_nacho(se), class = "nacho_error_bad_argument")
  expect_snapshot(
    as_nacho(se_with_setting("normalisation_method", "foo")),
    error = TRUE
  )
})

test_that("as_nacho() checks the thresholds and RCC type saved in the metadata", {
  skip_if_not_installed("SummarizedExperiment")
  se <- se_with_nacho_metadata("thresholds", list(a = 1))
  expect_error(as_nacho(se), class = "nacho_error_bad_argument")
  se <- se_with_nacho_metadata("rcc_type", "n2")
  expect_error(as_nacho(se), class = "nacho_error_bad_argument")
  expect_snapshot(as_nacho(se), error = TRUE)
})

test_that("a missing Bioconductor package gives the install command", {
  local_mocked_bindings(has_package = function(package) FALSE)
  expect_error(
    as_summarized_experiment(GSE74821),
    class = "nacho_error_missing_package"
  )
  expect_snapshot(as_summarized_experiment(GSE74821), error = TRUE)
})

rcc_set_files <- function() {
  directory <- system.file(
    "extdata",
    "3D_Bio_Example_Data",
    package = "NanoStringNCTools"
  )
  dir(directory, pattern = "^SKMEL.*RCC$", full.names = TRUE)
}

test_that("as_nacho() on a NanoStringRccSet matches load_rcc() on the same files", {
  skip_if_not_installed("NanoStringNCTools")
  files <- rcc_set_files()
  rccset <- NanoStringNCTools::readNanoStringRccSet(files)
  from_rccset <- suppressMessages(as_nacho(rccset))
  from_files <- suppressMessages(
    load_rcc(
      dirname(files[[1]]),
      data.frame(IDFILE = basename(files)),
      "IDFILE"
    )
  )
  common <- intersect(
    rownames(nacho_counts(from_rccset)),
    rownames(nacho_counts(from_files))
  )
  expect_gt(length(common), 0.9 * nrow(from_files))
  expect_identical(
    nacho_counts(from_rccset)[common, basename(files)],
    nacho_counts(from_files)[common, basename(files)]
  )
  expect_identical(nacho_samples(from_rccset)[["IDFILE"]], basename(files))
  expect_equal(nacho_qc(from_rccset)$FoV, nacho_qc(from_files)$FoV)
  expect_equal(nacho_qc(from_rccset)$PCL, nacho_qc(from_files)$PCL)
  expect_identical(nacho_qc(from_rccset)$Date, nacho_qc(from_files)$Date)
  expect_identical(nacho_qc(from_rccset)$ID, nacho_qc(from_files)$ID)
})

test_that("as_nacho() on a NanoStringRccSet needs NanoStringNCTools", {
  skip_if_not_installed("NanoStringNCTools")
  rccset <- NanoStringNCTools::readNanoStringRccSet(rcc_set_files())
  local_mocked_bindings(has_package = function(package) FALSE)
  expect_error(as_nacho(rccset), class = "nacho_error_missing_package")
})

test_that("as_nacho() needs a counts assay", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  SummarizedExperiment::assayNames(se) <- c("raw", "normalised")
  expect_error(as_nacho(se), class = "nacho_error_bad_object")
})

test_that("as_nacho() keeps the missing counts of probes absent from some files", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  endogenous <- which(
    SummarizedExperiment::rowData(se)$CodeClass == "Endogenous"
  )
  counts <- SummarizedExperiment::assay(se, "counts")
  counts[endogenous[[1]], 1] <- NA_integer_
  SummarizedExperiment::assay(se, "counts") <- counts
  expect_warning(
    x <- suppressMessages(as_nacho(se)),
    class = "nacho_warning_missing_counts"
  )
  expect_identical(nacho_counts(x), counts)
  expect_warning(
    round_trip <- suppressMessages(as_nacho(as_summarized_experiment(x))),
    class = "nacho_warning_missing_counts"
  )
  expect_identical(nacho_counts(round_trip), counts)
  expect_equal(nacho_qc(round_trip), nacho_qc(x))
})

test_that("as_nacho() drops saved housekeeping genes the rows no longer hold", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  keep <- SummarizedExperiment::rowData(se)$CodeClass != "Housekeeping"
  expect_warning(
    x <- suppressMessages(as_nacho(se[keep, ])),
    class = "nacho_warning_no_housekeeping"
  )
  expect_null(x@settings$housekeeping_genes)
  expect_false(x@settings$housekeeping_norm)
  expect_false(anyNA(nacho_counts(x, normalised = TRUE)))
})

test_that("as_nacho() fills missing settings with the defaults", {
  skip_if_not_installed("SummarizedExperiment")
  se <- se_with_nacho_metadata("settings", list(id_colname = "IDFILE"))
  x <- suppressMessages(as_nacho(se))
  expect_identical(x@settings$normalisation_method, "GEO")
  expect_identical(x@settings$n_comp, 10L)
  expect_true(x@settings$housekeeping_norm)
})

test_that("as_nacho() checks the provenance saved in the metadata", {
  skip_if_not_installed("SummarizedExperiment")
  se <- se_with_nacho_metadata("provenance", "old")
  expect_error(as_nacho(se), class = "nacho_error_bad_argument")
  expect_snapshot(as_nacho(se), error = TRUE)
})

test_that("as_nacho() does not overwrite a different sample id column", {
  skip_if_not_installed("SummarizedExperiment")
  se <- as_summarized_experiment(GSE74821)
  S4Vectors::metadata(se) <- list()
  se$IDFILE <- rev(se$IDFILE)
  expect_error(as_nacho(se), class = "nacho_error_bad_argument")
  expect_snapshot(as_nacho(se), error = TRUE)
  x <- suppressMessages(as_nacho(se, id_colname = "sample"))
  expect_identical(nacho_samples(x)$sample, colnames(se))
  expect_identical(nacho_samples(x)$IDFILE, rev(colnames(se)))
})

test_that("default_settings() follows the load_rcc() defaults", {
  defaults <- lapply(
    as.list(formals(load_rcc))[c(
      "housekeeping_genes",
      "housekeeping_predict",
      "normalisation_method",
      "n_comp"
    )],
    eval
  )
  defaults$n_comp <- as.integer(defaults$n_comp)
  settings <- NACHO:::default_settings(nacho_probes(GSE74821), "IDFILE")
  expect_identical(settings[names(defaults)], defaults)
  expect_identical(formals(load_rcc)$housekeeping_norm, TRUE)
})
