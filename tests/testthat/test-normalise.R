test_that("default settings", {
  res <- normalise(
    nacho_object = GSE74821
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
})

test_that("missing nacho", {
  expect_error(normalise(), class = "nacho_error_bad_object")
})

test_that("normalise() refuses a NACHO 2 list", {
  old <- structure(list(nacho = data.frame()), class = "nacho")
  expect_error(normalise(old), class = "nacho_error_bad_object")
})

test_that("normalise() checks its arguments", {
  expect_error(
    normalise(GSE74821, normalisation_method = "geo"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    normalise(GSE74821, n_comp = -1),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    normalise(GSE74821, housekeeping_norm = "yes"),
    class = "nacho_error_bad_argument"
  )
})

test_that("normalise() says when nothing changes", {
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  expect_message(normalise(GSE74821), class = "nacho_message")
})

test_that("No POS_E", {
  no_pos_e <- GSE74821[nacho_probes(GSE74821)[["Name"]] != "POS_E(0.5)", ]
  res <- normalise(
    nacho_object = no_pos_e,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_false("POS_E(0.5)" %in% nacho_probes(res)[["Name"]])
})

test_that("genes not null", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = c("RPLP0", "ACTB"),
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_setequal(res@settings[["housekeeping_genes"]], c("RPLP0", "ACTB"))
  probes <- nacho_probes(res)
  expect_setequal(
    probes[["Name"]][probes[["is_housekeeping"]]],
    c("RPLP0", "ACTB")
  )
})

test_that("predict TRUE", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = TRUE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_true(res@settings[["housekeeping_predict"]])
})

test_that("norm TRUE", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = TRUE,
    normalisation_method = "GEO",
    n_comp = 10,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_true(res@settings[["housekeeping_norm"]])
})

test_that("method GLM", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GLM",
    n_comp = 10,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_identical(res@settings[["normalisation_method"]], "GLM")
})

test_that("n_comp 2", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 2,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_identical(ncol(res@pca[["scores"]]), 2L)
})

test_that("n_comp 10", {
  res <- normalise(
    nacho_object = GSE74821,
    housekeeping_genes = NULL,
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_identical(ncol(res@pca[["scores"]]), 10L)
})

test_that("All LoD to zero", {
  zero_lod <- GSE74821
  samples <- zero_lod@samples
  samples[["LoD"]] <- 0
  zero_lod@samples <- samples
  res <- normalise(
    nacho_object = zero_lod,
    housekeeping_genes = c("RPLP0", "ACTB"),
    housekeeping_predict = FALSE,
    housekeeping_norm = FALSE,
    normalisation_method = "GEO",
    n_comp = 10,
    outliers_thresholds = NACHO:::default_thresholds()
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
})

test_that("Missing values in counts", {
  with_na <- GSE74821
  endogenous <- which(nacho_probes(with_na)[["CodeClass"]] == "Endogenous")
  counts <- with_na@counts
  counts[sample(endogenous, size = 25), 1] <- NA_integer_
  with_na@counts <- counts
  expect_warning(
    object = normalise(with_na, normalisation_method = "GEO"),
    class = "nacho_warning_missing_counts"
  ) |>
    suppressMessages()
})

test_that("plexset", {
  res <- normalise(
    plexset_nacho,
    housekeeping_predict = TRUE,
    housekeeping_norm = TRUE
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_identical(res@rcc_type, "n8")
})

test_that("plexset GLM", {
  res <- normalise(
    plexset_nacho,
    housekeeping_predict = TRUE,
    housekeeping_norm = TRUE,
    normalisation_method = "GLM"
  )
  expect_true(S7::S7_inherits(res, NACHO:::nacho))
  expect_identical(res@settings[["normalisation_method"]], "GLM")
})

test_that("normalise() uses the n_comp it receives", {
  res <- suppressMessages(normalise(GSE74821, n_comp = 3))
  expect_identical(res@settings[["n_comp"]], 3L)
  expect_identical(nrow(res@pca[["importance"]]), 3L)
  expect_identical(
    grep("^PC[0-9]+$", names(nacho_samples(res)), value = TRUE),
    sprintf("PC%02d", 1:3)
  )
})

test_that("exclude_outliers() keeps the requested n_comp", {
  thresholds <- GSE74821@thresholds
  thresholds[["FoV"]] <- 99.5
  flagged <- suppressMessages(normalise(
    GSE74821,
    n_comp = 4,
    outliers_thresholds = thresholds
  ))
  res <- suppressMessages(exclude_outliers(flagged))
  expect_identical(res@settings[["n_comp"]], 4L)
  expect_identical(nrow(res@pca[["importance"]]), 4L)
})

test_that("normalise() flags outliers against new thresholds", {
  thresholds <- GSE74821@thresholds
  thresholds[["BD"]] <- c(0.1, 0.2)
  res <- suppressMessages(normalise(GSE74821, outliers_thresholds = thresholds))
  expect_identical(
    nacho_qc(res)[["is_outlier"]],
    nacho_qc(check_outliers(res))[["is_outlier"]]
  )
  expect_true(any(nacho_qc(res)[["is_outlier"]]))
})

test_that("a panel without POS_E gives NA for PCL and LoD, not a failure", {
  no_pos_e <- GSE74821[nacho_probes(GSE74821)[["Name"]] != "POS_E(0.5)", ]
  res <- suppressMessages(normalise(no_pos_e, normalisation_method = "GEO"))
  expect_true(all(is.na(nacho_qc(res)[["PCL"]])))
  expect_true(all(is.na(nacho_qc(res)[["LoD"]])))
  expect_false(anyNA(nacho_qc(res)[["is_outlier"]]))
})

test_that("normalise() returns a nacho object", {
  x <- suppressMessages(normalise(salmon_nacho, normalisation_method = "GLM"))
  expect_true(S7::S7_inherits(x, NACHO:::nacho))
  expect_identical(x@settings$normalisation_method, "GLM")
})

test_that("normalise() with new thresholds only recomputes the flags", {
  tight <- salmon_nacho@thresholds
  tight$BD <- c(0.5, 0.6)
  x <- suppressMessages(normalise(salmon_nacho, outliers_thresholds = tight))
  expect_identical(x@thresholds, tight)
  expect_identical(
    nacho_counts(x, normalised = TRUE),
    nacho_counts(salmon_nacho, normalised = TRUE)
  )
  expect_identical(
    nacho_qc(x)$is_outlier,
    NACHO:::compute_outliers(x@samples, tight, x@rcc_type)
  )
})

test_that("normalise() refuses remove_outliers and points to exclude_outliers()", {
  expect_error(
    normalise(salmon_nacho, remove_outliers = TRUE),
    class = "nacho_error_bad_argument"
  )
  expect_snapshot(normalise(salmon_nacho, remove_outliers = TRUE), error = TRUE)
})

test_that("normalise() refuses insane thresholds", {
  bad <- salmon_nacho@thresholds
  bad$FoV <- 150
  expect_error(
    normalise(salmon_nacho, outliers_thresholds = bad),
    class = "nacho_error_bad_argument"
  )
})

test_that("normalise() refuses infinite thresholds", {
  bad <- salmon_nacho@thresholds
  bad$LoD <- Inf
  expect_error(
    normalise(salmon_nacho, outliers_thresholds = bad),
    class = "nacho_error_bad_argument"
  )
  bad <- salmon_nacho@thresholds
  bad$BD <- c(0.1, Inf)
  expect_error(
    normalise(salmon_nacho, outliers_thresholds = bad),
    class = "nacho_error_bad_argument"
  )
})

test_that("exclude_outliers() drops every flagged sample and normalises the rest", {
  tight <- plexset_nacho@thresholds
  tight$Positive_factor <- c(0.9, 1.1)
  flagged <- suppressMessages(normalise(
    plexset_nacho,
    outliers_thresholds = tight
  ))
  n_flagged <- sum(nacho_qc(flagged)$is_outlier)
  skip_if(
    n_flagged == 0 || n_flagged == ncol(flagged),
    "Tight thresholds flag no sample or every sample."
  )
  kept <- suppressMessages(exclude_outliers(flagged))
  expect_identical(ncol(kept), ncol(flagged) - n_flagged)
  expect_false(any(
    nacho_samples(kept)$IDFILE %in%
      nacho_samples(flagged)$IDFILE[nacho_qc(flagged)$is_outlier]
  ))
  expect_identical(kept@thresholds, tight)
})

test_that("exclude_outliers() refuses to drop every sample", {
  flagged <- plexset_nacho
  samples <- flagged@samples
  samples$is_outlier <- TRUE
  flagged@samples <- samples
  expect_error(exclude_outliers(flagged), class = "nacho_error_bad_argument")
})

test_that("check_outliers() recomputes the flags from the thresholds", {
  x <- salmon_nacho
  samples <- x@samples
  samples$is_outlier <- !samples$is_outlier
  x@samples <- samples
  expect_identical(
    nacho_qc(check_outliers(x))$is_outlier,
    nacho_qc(salmon_nacho)$is_outlier
  )
})

test_that("the missing attributes warning comes once, when the object is built", {
  samples <- GSE74821@samples
  samples <- samples[, !grepl("^Lane_Attributes", names(samples))]
  count_unavailable <- function(expr) {
    n <- 0L
    value <- withCallingHandlers(
      expr,
      nacho_warning_metric_unavailable = function(cnd) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, n = n)
  }
  built <- count_unavailable(NACHO:::build_nacho(
    counts = GSE74821@counts,
    probes = GSE74821@probes,
    samples = samples,
    settings = GSE74821@settings,
    thresholds = GSE74821@thresholds,
    rcc_type = GSE74821@rcc_type,
    provenance = GSE74821@provenance
  ))
  expect_identical(built$n, 1L)
  x <- built$value
  expect_no_warning(suppressMessages(normalise(x, n_comp = 5)))
  expect_no_warning(suppressMessages(normalise(
    x,
    normalisation_method = "GEO"
  )))
  flagged <- x
  flagged_samples <- flagged@samples
  flagged_samples$is_outlier <- seq_len(nrow(flagged_samples)) == 1L
  flagged@samples <- flagged_samples
  expect_no_warning(suppressMessages(exclude_outliers(flagged)))
  expect_no_warning(check_outliers(x))
  expect_no_warning(x[, 1:20])
})
