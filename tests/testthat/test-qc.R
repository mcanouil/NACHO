test_that("geometric_means() sets zeros to 1 and ignores missing probes", {
  m <- matrix(c(0, 4, NA, 1, 4, 16), nrow = 3)
  expect_equal(NACHO:::geometric_means(m), c(2, 4))
})

test_that("excluded_negatives() drops negatives far from the overall median", {
  counts <- matrix(
    c(10, 10, 10, 40, 10, 10, 11, 42),
    nrow = 4,
    dimnames = list(as.character(1:4), c("a", "b"))
  )
  expect_identical(NACHO:::excluded_negatives(counts, rep("Negative", 4)), "4")
})

test_that("normalise_matrix() rounds, then floors at 0.1", {
  counts <- matrix(c(10, 2, 30, 1), nrow = 2)
  out <- NACHO:::normalise_matrix(
    counts,
    negative_factor = c(3, 3),
    positive_factor = c(1.5, 1),
    house_factor = NULL
  )
  expect_identical(out, matrix(c(10, 0.1, 27, 0.1), nrow = 2))
})

test_that("sample_metrics() gives NA and one warning when lane attributes are missing", {
  x <- GSE74821
  samples <- x@samples[, !grepl("^Lane_Attributes", names(x@samples))]
  warnings <- list()
  metrics <- withCallingHandlers(
    NACHO:::sample_metrics(x@counts, x@probes, samples),
    nacho_warning_metric_unavailable = function(cnd) {
      warnings <<- c(warnings, list(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1)
  expect_true(all(is.na(metrics$FoV)))
  expect_true(all(is.na(metrics$BD)))
  expect_false(anyNA(metrics$MC))
})

test_that("missing probes in some files keep QC working", {
  fixture <- geo_fixture("GSE178516")
  directory <- withr::local_tempdir()
  files <- file.path(fixture$dir, fixture$samplesheet$IDFILE[1:3])
  file.copy(files, directory)
  target <- file.path(directory, basename(files[1]))
  lines <- readLines(target)
  lines <- lines[-grep("^Endogenous,", lines)[1]]
  connection <- gzfile(target, "w")
  writeLines(lines, connection)
  close(connection)
  expect_warning(
    x <- suppressMessages(load_rcc(
      directory,
      fixture$samplesheet[1:3, ],
      "IDFILE",
      n_comp = 2
    )),
    class = "nacho_warning_missing_counts"
  )
  expect_identical(sum(is.na(nacho_counts(x))), 1L)
  expect_true(all(is.finite(nacho_qc(x)$MC)))
  expect_true(all(is.finite(nacho_qc(x)$Positive_factor)))
})
