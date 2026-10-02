test_that("geometric_means() sets zeros to 1 and ignores missing probes", {
  m <- matrix(c(0, 4, NA, 1, 4, 16), nrow = 3)
  expect_equal(NACHO:::geometric_means(m), c(2, 4))
})

negative_matrix <- function(means) {
  matrix(
    rep(means, times = 2),
    ncol = 2,
    dimnames = list(sprintf("NEG_%s", LETTERS[seq_along(means)]), c("S1", "S2"))
  )
}

test_that("the Bruker rule drops at most two negatives 3-fold above the others", {
  exclude <- function(means) {
    NACHO:::excluded_negatives(
      negative_matrix(means),
      rep("Negative", length(means)),
      "nsolver"
    )
  }
  expect_identical(exclude(c(10, 11, 9, 12, 8, 10)), character(0))
  expect_identical(exclude(c(40, 11, 9, 12, 8, 10)), "NEG_A")
  expect_identical(exclude(c(40, 50, 9, 12, 8, 10)), c("NEG_B", "NEG_A"))
  expect_identical(exclude(c(40, 50, 60, 12, 8, 10)), character(0))
  expect_identical(exclude(c(40, 11, 9)), character(0))
})

test_that("a negative probe with only missing counts is left out of the rule", {
  counts <- negative_matrix(c(40, 11, 9, 12))
  counts[c("NEG_C", "NEG_D"), ] <- NA
  expect_identical(
    NACHO:::excluded_negatives(counts, rep("Negative", 4), "nsolver"),
    character(0)
  )
  padded <- negative_matrix(c(40, 11, 9, 12, 10))
  padded["NEG_E", ] <- NA
  expect_identical(
    NACHO:::excluded_negatives(padded, rep("Negative", 5), "nsolver"),
    "NEG_A"
  )
})

test_that("the legacy rule keeps the NACHO 2 median rule", {
  counts <- matrix(
    c(10, 10, 10, 30, 10, 11, 9, 31),
    ncol = 2,
    dimnames = list(1:4, 1:2)
  )
  expect_identical(
    NACHO:::excluded_negatives(counts, rep("Negative", 4), "legacy"),
    "4"
  )
})

test_that("the legacy rule never reports a missing negative probe name", {
  counts <- matrix(
    c(10, 10, 10, NA, NA, NA, 10, 11, 9, 30, 31, 29),
    nrow = 4,
    byrow = TRUE,
    dimnames = list(c("A", "B", "C", "D"), 1:3)
  )
  expect_identical(
    NACHO:::excluded_negatives(counts, rep("Negative", 4), "legacy"),
    "D"
  )
})

test_that("nsolver PCL leaves POS_F out and adds 1 to every count", {
  names <- sprintf("POS_%s(%s)", LETTERS[1:6], c(128, 32, 8, 2, 0.5, 0.125))
  positives <- matrix(c(1000, 260, 70, 15, 6, 0), ncol = 1)
  expected <- stats::cor(
    log2(positives[1:5] + 1),
    log2(c(128, 32, 8, 2, 0.5))
  )^2
  expect_equal(
    NACHO:::sample_pcl(positives, names, "nsolver"),
    round(expected, 5)
  )
  legacy <- stats::cor(
    log2(as.vector(positives) + 1),
    log2(c(128, 32, 8, 2, 0.5, 0.125))
  )^2
  expect_equal(NACHO:::sample_pcl(positives, names, "legacy"), round(legacy, 5))
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
    NACHO:::sample_metrics(x@counts, x@probes, samples, "legacy"),
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

test_that("a negative probe with all counts missing is never excluded", {
  exclude <- function(counts, preset) {
    NACHO:::excluded_negatives(counts, rep("Negative", nrow(counts)), preset)
  }
  counts <- negative_matrix(c(40, 11, 9, 12, 10))
  counts["NEG_E", ] <- NA
  expect_false("NEG_E" %in% expect_no_error(exclude(counts, "legacy")))

  few <- negative_matrix(c(40, 11, 9, 12))
  few["NEG_D", ] <- NA
  expect_false("NEG_D" %in% expect_no_error(exclude(few, "legacy")))

  none <- negative_matrix(c(40, 11, 9, 12))
  none[] <- NA
  for (preset in c("nsolver", "legacy")) {
    expect_identical(expect_no_error(exclude(none, preset)), character(0))
  }
})
