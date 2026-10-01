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

test_that("normalised counts are neither rounded nor floored", {
  counts <- matrix(
    c(3L, 7L, 11L, 2L),
    ncol = 2,
    dimnames = list(c("a", "b"), c("S1", "S2"))
  )
  scaled <- NACHO:::scale_counts(
    counts,
    background = c(4, 1),
    background_mode = "subtract",
    positive_factor = c(1.5, 0.5)
  )
  expect_identical(
    scaled,
    matrix(c(0, 4.5, 5, 0.5), ncol = 2, dimnames = dimnames(counts))
  )
  expect_identical(
    NACHO:::scale_counts(counts, NULL, "threshold", c(1.5, 0.5)),
    matrix(c(4.5, 10.5, 5.5, 1), ncol = 2, dimnames = dimnames(counts))
  )
})

test_that("content_factor() floors at 1 before the geometric mean", {
  rows <- matrix(c(0.5, 4, 2, 8), ncol = 2)
  g <- c(exp(mean(log(c(1, 4)))), exp(mean(log(c(2, 8)))))
  expect_equal(NACHO:::content_factor(rows), mean(g) / g)
})

test_that("content_factor() keeps one missing sample from spoiling the others", {
  rows <- matrix(c(4, 2, NA, NA, 8, 2), ncol = 3)
  factor <- NACHO:::content_factor(rows)
  expect_true(is.nan(factor[[2]]))
  expect_false(anyNA(factor[-2]))
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
  expect_match(
    conditionMessage(warnings[[1]]),
    "Lane_Attributes.lane_BindingDensity",
    fixed = TRUE
  )
  expect_named(
    metrics,
    c(
      "Date",
      "ID",
      "BD",
      "ScannerID",
      "StagePosition",
      "CartridgeID",
      "FoV",
      "PCL",
      "LoD",
      "MC",
      "MedC"
    )
  )
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
