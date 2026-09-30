detection_counts <- function() {
  counts <- matrix(
    c(
      10L,
      10L,
      12L,
      14L,
      14L,
      18L,
      100L,
      5L,
      50L,
      60L,
      15L,
      21L
    ),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(
      c("NEG_A", "NEG_B", "NEG_C", "G1", "G2", "HK"),
      c("S1", "S2")
    )
  )
  list(
    counts = counts,
    code_class = c(
      rep("Negative", 3),
      "Endogenous",
      "Endogenous",
      "Housekeeping"
    )
  )
}

test_that("detection limits are the mean plus two SD of the kept negatives", {
  d <- detection_counts()
  expect_equal(
    NACHO:::detection_limits(d$counts, d$code_class, character(0)),
    c(12 + 2 * 2, 14 + 2 * 4)
  )
})

test_that("detection rates count endogenous genes above the limit", {
  d <- detection_counts()
  limits <- c(16, 22)
  hits <- NACHO:::detected(d$counts, limits)
  expect_identical(unname(hits["G1", ]), c(TRUE, FALSE))
  expect_identical(unname(hits["HK", ]), c(FALSE, FALSE))
})

test_that("no negatives gives missing detection limits", {
  d <- detection_counts()
  expect_identical(
    NACHO:::detection_limits(d$counts, rep("Endogenous", 6), character(0)),
    c(NA_real_, NA_real_)
  )
})

test_that("nacho objects carry sample and probe detection rates", {
  samples <- nacho_samples(GSE74821)
  probes <- nacho_probes(GSE74821)
  expect_true(all(samples$Detection_rate >= 0 & samples$Detection_rate <= 1))
  expect_true(all(probes$detection_rate >= 0 & probes$detection_rate <= 1))
})

test_that("filter_detected() keeps controls and genes detected often enough", {
  x <- suppressWarnings(filter_detected(GSE74821, min_rate = 0.9))
  probes <- nacho_probes(x)
  endogenous <- grepl("Endogenous", probes$CodeClass)
  expect_true(all(probes$detection_rate[endogenous] >= 0.9))
  expect_identical(
    sum(!grepl("Endogenous", nacho_probes(GSE74821)$CodeClass)),
    sum(!endogenous)
  )
  expect_error(
    filter_detected(GSE74821, min_rate = 2),
    class = "nacho_error_bad_argument"
  )
})

test_that("check_proportion() accepts numbers from 0 to 1 only", {
  expect_invisible(NACHO:::check_proportion(0.5))
  expect_error(
    NACHO:::check_proportion(-0.1),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::check_proportion(NA_real_),
    class = "nacho_error_bad_argument"
  )
})
