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
  before <- nacho_probes(GSE74821)
  min_rate <- max(before$detection_rate, na.rm = TRUE)
  x <- filter_detected(GSE74821, min_rate = min_rate)
  probes <- nacho_probes(x)
  endogenous <- grepl("Endogenous", probes$CodeClass)
  expect_lt(
    sum(grepl("Endogenous", probes$CodeClass)),
    sum(grepl("Endogenous", before$CodeClass))
  )
  expect_true(all(probes$detection_rate[endogenous] >= min_rate))
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
    NACHO:::check_proportion(2),
    "not 2",
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::check_proportion(NA_real_),
    class = "nacho_error_bad_argument"
  )
})

rebuild_with_counts <- function(x, counts, probes, warn_missing = TRUE) {
  suppressWarnings(
    NACHO:::build_nacho(
      counts = counts,
      probes = probes,
      samples = x@samples[seq_len(ncol(counts)), , drop = FALSE],
      settings = x@settings,
      thresholds = x@thresholds,
      rcc_type = x@rcc_type,
      provenance = x@provenance,
      warn_missing = warn_missing
    ),
    classes = c("nacho_warning_n_comp_reduced", "nacho_warning_missing_counts")
  )
}

count_unavailable <- function(expr) {
  n <- 0L
  messages <- character(0)
  value <- withCallingHandlers(
    expr,
    nacho_warning_metric_unavailable = function(cnd) {
      n <<- n + 1L
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, n = n, messages = messages)
}

test_that("a build without negative probes gives missing detection rates and one warning", {
  keep <- GSE74821@probes$CodeClass != "Negative"
  counts <- GSE74821@counts[keep, 1:3]
  probes <- GSE74821@probes[keep, ]
  built <- count_unavailable(rebuild_with_counts(GSE74821, counts, probes))
  expect_identical(built$n, 1L)
  expect_true(all(is.na(nacho_samples(built$value)$Detection_rate)))
  expect_true(all(is.na(nacho_probes(built$value)$detection_rate)))
  expect_false(any(is.nan(nacho_probes(built$value)$detection_rate)))
  quiet <- count_unavailable(rebuild_with_counts(
    GSE74821,
    counts,
    probes,
    FALSE
  ))
  expect_identical(quiet$n, 0L)
})

test_that("a sample without a detection limit gets NA, never NaN, with one warning", {
  counts <- GSE74821@counts[, 1:3]
  negative <- GSE74821@probes$CodeClass == "Negative"
  counts[negative, 2] <- NA
  built <- count_unavailable(rebuild_with_counts(
    GSE74821,
    counts,
    GSE74821@probes
  ))
  expect_identical(built$n, 1L)
  rate <- nacho_samples(built$value)$Detection_rate
  expect_true(is.na(rate[2]) && !is.nan(rate[2]))
  expect_false(anyNA(rate[-2]))
  expect_false(any(is.nan(nacho_probes(built$value)$detection_rate)))
  expect_false(anyNA(nacho_probes(built$value)$detection_rate))
})

test_that("fewer than three detected housekeeping genes flags the sample", {
  x <- GSE74821
  qc <- nacho_qc(x)
  expect_true("Housekeeping_detected_status" %in% names(qc))
  x@samples$Housekeeping_detected[1] <- 2L
  expect_identical(nacho_qc(x)$Housekeeping_detected_status[1], "fail")
  x@thresholds <- nacho_thresholds(preset = "legacy")
  expect_identical(nacho_qc(x)$Housekeeping_detected_status[1], "pass")
})

test_that("Housekeeping_detected counts housekeeping genes above the limit", {
  samples <- nacho_samples(GSE74821)
  probes <- nacho_probes(GSE74821)
  expected <- colSums(
    NACHO:::detected(
      GSE74821@counts,
      NACHO:::detection_limits(
        GSE74821@counts,
        probes$CodeClass,
        probes$Name[probes$is_excluded]
      )
    )[probes$is_housekeeping, , drop = FALSE]
  )
  expect_identical(samples$Housekeeping_detected, unname(as.integer(expected)))
})

test_that("Housekeeping_detected is NA per sample without a limit and without housekeeping genes", {
  counts <- GSE74821@counts[, 1:3]
  negative <- GSE74821@probes$CodeClass == "Negative"
  counts[negative, 2] <- NA
  built <- suppressWarnings(rebuild_with_counts(
    GSE74821,
    counts,
    GSE74821@probes
  ))
  found <- nacho_samples(built)$Housekeeping_detected
  expect_true(is.na(found[2]))
  expect_false(anyNA(found[-2]))
  expect_type(found, "integer")

  probes <- GSE74821@probes
  probes$CodeClass[grepl("Housekeeping", probes$CodeClass)] <- "Endogenous"
  bare <- GSE74821
  bare@settings[["housekeeping_genes"]] <- NULL
  none <- rebuild_with_counts(bare, GSE74821@counts, probes)
  expect_identical(
    nacho_samples(none)$Housekeeping_detected,
    rep(NA_integer_, ncol(GSE74821@counts))
  )
})

test_that("the legacy preset passes a sample with no housekeeping genes detected", {
  x <- GSE74821
  x@samples$Housekeeping_detected[1] <- 0L
  x@thresholds <- nacho_thresholds(preset = "legacy")
  expect_identical(nacho_qc(x)$Housekeeping_detected_status[1], "pass")
  x@thresholds <- nacho_thresholds(preset = "nsolver")
  expect_identical(nacho_qc(x)$Housekeeping_detected_status[1], "fail")
})

test_that("detection rates and housekeeping counts match hand-computed values", {
  fixture <- detection_counts()
  pick <- c(
    which(GSE74821@probes$CodeClass == "Negative")[1:3],
    which(GSE74821@probes$CodeClass == "Endogenous")[1:2],
    which(GSE74821@probes$CodeClass == "Housekeeping")[1]
  )
  probes <- GSE74821@probes[pick, ]
  counts <- fixture$counts
  rownames(counts) <- probes$Name
  colnames(counts) <- colnames(GSE74821@counts)[1:2]
  x <- GSE74821
  x@settings$housekeeping_genes <- probes$Name[6]
  x@settings$normalisation_method <- "GEO"
  built <- suppressWarnings(rebuild_with_counts(x, counts, probes))
  expect_identical(nacho_samples(built)$Detection_rate, c(1, 0.5))
  expect_identical(nacho_probes(built)$detection_rate, c(0, 0, 0, 0.5, 1, 0))
  expect_identical(nacho_samples(built)$Housekeeping_detected, c(0L, 0L))
  counts[6, ] <- c(17L, 23L)
  built <- suppressWarnings(rebuild_with_counts(x, counts, probes))
  expect_identical(nacho_samples(built)$Housekeeping_detected, c(1L, 1L))
  counts[6, ] <- c(17L, 21L)
  built <- suppressWarnings(rebuild_with_counts(x, counts, probes))
  expect_identical(nacho_samples(built)$Housekeeping_detected, c(1L, 0L))
})

test_that("filter_detected() refuses an object with no detection rates", {
  x <- GSE74821
  x@probes$detection_rate <- NA_real_
  expect_error(filter_detected(x), class = "nacho_error_no_detection_rate")
})

test_that("filter_detected() returns an object without endogenous genes unchanged", {
  keep <- which(GSE74821@probes$CodeClass != "Endogenous")
  x <- suppressWarnings(GSE74821[keep, ])
  expect_identical(filter_detected(x), x)
})

test_that("Housekeeping_detected is NA when no housekeeping gene matches a probe", {
  x <- GSE74821
  x@settings$housekeeping_genes <- "NOT_A_PROBE"
  built <- suppressWarnings(rebuild_with_counts(x, x@counts, x@probes))
  expect_true(all(is.na(nacho_samples(built)$Housekeeping_detected)))
})

test_that("subsetting samples recomputes the probe detection rates", {
  keep <- 1:5
  x <- suppressWarnings(
    GSE74821[, keep],
    classes = "nacho_warning_n_comp_reduced"
  )
  probes <- nacho_probes(x)
  counts <- nacho_counts(GSE74821)[, keep]
  all_probes <- nacho_probes(GSE74821)
  limits <- NACHO:::detection_limits(
    counts,
    all_probes$CodeClass,
    all_probes$Name[all_probes$is_excluded]
  )
  expected <- NACHO:::missing_not_nan(rowMeans(
    NACHO:::detected(counts, limits),
    na.rm = TRUE
  ))
  expect_equal(probes$detection_rate, expected)
  expect_false(isTRUE(all.equal(
    probes$detection_rate,
    all_probes$detection_rate
  )))

  endogenous <- grepl("Endogenous", probes$CodeClass)
  filtered <- suppressWarnings(
    filter_detected(x, min_rate = 1),
    classes = "nacho_warning_n_comp_reduced"
  )
  kept <- nacho_probes(filtered)
  expect_identical(
    sum(grepl("Endogenous", kept$CodeClass)),
    sum(expected[endogenous] >= 1)
  )
  expect_true(all(
    kept$detection_rate[grepl("Endogenous", kept$CodeClass)] == 1
  ))
})

test_that("subsetting probes keeps the detection rates of the samples kept", {
  x <- GSE74821[1:20, ]
  expect_equal(
    nacho_probes(x)$detection_rate,
    nacho_probes(GSE74821)$detection_rate[1:20]
  )
})

test_that("a missing background warns once, names the sample, and stays quiet on rebuilds", {
  x <- GSE74821
  x@settings$background <- "geo"
  counts <- x@counts[, 1:3]
  counts[x@probes$CodeClass == "Negative", 2] <- NA
  loud <- count_unavailable(rebuild_with_counts(x, counts, x@probes))
  background <- grep("background", loud$messages, value = TRUE)
  expect_length(background, 1L)
  expect_match(background, colnames(counts)[2], fixed = TRUE)
  quiet <- count_unavailable(rebuild_with_counts(x, counts, x@probes, FALSE))
  expect_identical(quiet$n, 0L)
})

test_that("build_nacho() errors from background name the caller", {
  x <- GSE74821
  x@settings$background <- "mean_2sd"
  keep <- x@probes$CodeClass != "Negative"
  counts <- x@counts[keep, 1:3]
  error <- tryCatch(
    NACHO:::build_nacho(
      counts = counts,
      probes = x@probes[keep, ],
      samples = x@samples[1:3, , drop = FALSE],
      settings = x@settings,
      thresholds = x@thresholds,
      rcc_type = x@rcc_type,
      provenance = x@provenance,
      call = rlang::call2("normalise")
    ),
    error = identity
  )
  expect_s3_class(error, "nacho_error_bad_argument")
  expect_identical(rlang::call_name(error$call), "normalise")
})

test_that("with all-zero negatives every non-zero count counts as detected", {
  counts <- matrix(
    c(0, 0, 0, 0, 1, 5, 0, 3),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("NEG_A", "NEG_B", "G1", "G2"), c("S1", "S2"))
  )
  code_class <- c("Negative", "Negative", "Endogenous", "Endogenous")
  limits <- NACHO:::detection_limits(counts, code_class, character(0))
  expect_equal(limits, c(0, 0))
  hits <- NACHO:::detected(counts, limits)
  expect_true(hits["G1", "S1"])
  expect_false(hits["G2", "S1"])
})

test_that("filter_detected() keeps every gene with a non-zero count when negatives are all zero", {
  x <- GSE74821
  counts <- x@counts[, 1:3]
  counts[x@probes$CodeClass == "Negative", ] <- 0L
  zeroed <- which(x@probes$CodeClass == "Endogenous")[1:2]
  counts[zeroed[1], 1] <- 0L
  counts[zeroed[2], ] <- 0L
  built <- count_unavailable(rebuild_with_counts(x, counts, x@probes))$value
  endogenous <- x@probes$CodeClass == "Endogenous"
  expected <- x@probes$Name[endogenous][
    rowSums(counts[endogenous, ] > 0) == 3
  ]
  expect_gt(length(expected), 0L)
  expect_lt(length(expected), sum(endogenous))
  kept <- suppressWarnings(
    filter_detected(built, min_rate = 1),
    classes = "nacho_warning_n_comp_reduced"
  )
  kept_genes <- nacho_probes(kept)$Name[
    nacho_probes(kept)$CodeClass == "Endogenous"
  ]
  expect_setequal(kept_genes, expected)
})
