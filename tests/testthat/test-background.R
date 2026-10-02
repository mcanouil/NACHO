negatives_counts <- function() {
  counts <- matrix(
    c(
      10L,
      20L,
      12L,
      20L,
      14L,
      26L,
      100L,
      5L,
      50L,
      60L
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("NEG_A", "NEG_B", "NEG_C", "GENE1", "GENE2"),
      c("S1", "S2")
    )
  )
  list(
    counts = counts,
    code_class = c(
      "Negative",
      "Negative",
      "Negative",
      "Endogenous",
      "Endogenous"
    )
  )
}

test_that("background_levels() computes each statistic per sample", {
  d <- negatives_counts()
  level <- function(statistic, excluded = character(0)) {
    NACHO:::background_levels(d$counts, d$code_class, excluded, statistic)
  }
  expect_null(level("none"))
  expect_equal(level("mean"), c(12, 22))
  expect_equal(level("mean_2sd"), c(12 + 2 * 2, 22 + 2 * sqrt(12)))
  expect_equal(level("median"), c(12, 20))
  expect_equal(level("max"), c(14, 26))
  expect_equal(
    level("geo"),
    c(exp(mean(log(c(10, 12, 14)))), exp(mean(log(c(20, 20, 26)))))
  )
  expect_equal(level("max", excluded = "NEG_C"), c(12, 20))
})

test_that("background needs negatives", {
  d <- negatives_counts()
  expect_error(
    NACHO:::background_levels(
      d$counts,
      rep("Endogenous", 5),
      character(0),
      "mean"
    ),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::background_levels(
      d$counts,
      d$code_class,
      c("NEG_B", "NEG_C"),
      "mean_2sd"
    ),
    regexp = "two negative",
    class = "nacho_error_bad_argument"
  )
})

test_that("apply_background() thresholds or subtracts, and floors only at 0", {
  counts <- matrix(
    c(5L, 15L, 30L, 8L),
    ncol = 2,
    dimnames = list(c("a", "b"), c("S1", "S2"))
  )
  level <- c(10, 9)
  expect_identical(
    NACHO:::apply_background(counts, level, "threshold"),
    matrix(c(10, 15, 30, 9), ncol = 2, dimnames = dimnames(counts))
  )
  expect_identical(
    NACHO:::apply_background(counts, level, "subtract"),
    matrix(c(0, 5, 21, 0), ncol = 2, dimnames = dimnames(counts))
  )
  expect_identical(
    NACHO:::apply_background(counts, NULL, "threshold"),
    counts * 1
  )
})

test_that("missing counts stay missing", {
  counts <- matrix(
    c(NA, 15L, 30L, 8L),
    ncol = 2,
    dimnames = list(c("a", "b"), c("S1", "S2"))
  )
  for (mode in c("threshold", "subtract")) {
    out <- NACHO:::apply_background(counts, c(10, 9), mode)
    expect_true(is.na(out[1, 1]), info = mode)
    expect_false(anyNA(out[-1]), info = mode)
  }
})

test_that("a sample without any kept negative count gets an NA background and a warning", {
  d <- negatives_counts()
  d$counts[1:3, "S2"] <- NA
  for (statistic in c("geo", "mean", "mean_2sd", "median", "max")) {
    expect_warning(
      level <- NACHO:::background_levels(
        d$counts,
        d$code_class,
        character(0),
        statistic
      ),
      class = "nacho_warning_metric_unavailable",
      regexp = "S2"
    )
    expect_false(is.na(level[[1]]), label = statistic)
    expect_identical(level[[2]], NA_real_, label = statistic)
  }
  expect_no_warning(
    NACHO:::background_levels(
      d$counts,
      d$code_class,
      character(0),
      "geo",
      warn = FALSE
    )
  )
})

test_that("a sample with fewer than two observed negative counts gets an NA mean_2sd background", {
  d <- negatives_counts()
  d$counts[c("NEG_B", "NEG_C"), "S2"] <- NA
  level <- suppressWarnings(
    NACHO:::background_levels(d$counts, d$code_class, character(0), "mean_2sd")
  )
  expect_equal(level, c(12 + 2 * 2, NA))
  expect_false(is.nan(level[2]))
  d$counts[, "S2"] <- NA
  level <- suppressWarnings(
    NACHO:::background_levels(d$counts, d$code_class, character(0), "mean_2sd")
  )
  expect_identical(level[2], NA_real_)
})

test_that("background_levels() forwards the call it is given to its errors", {
  d <- negatives_counts()
  error <- tryCatch(
    NACHO:::background_levels(
      d$counts,
      d$code_class,
      c("NEG_B", "NEG_C"),
      "mean_2sd",
      call = rlang::call2("load_rcc")
    ),
    error = identity
  )
  expect_identical(rlang::call_name(error$call), "load_rcc")
})
