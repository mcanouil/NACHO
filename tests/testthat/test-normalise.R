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
    outliers_thresholds = nacho_thresholds()
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
    outliers_thresholds = nacho_thresholds()
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
    outliers_thresholds = nacho_thresholds()
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
    outliers_thresholds = nacho_thresholds()
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
    outliers_thresholds = nacho_thresholds()
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
    outliers_thresholds = nacho_thresholds()
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
    outliers_thresholds = nacho_thresholds()
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
    outliers_thresholds = nacho_thresholds()
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
  expect_true(any(nacho_qc(res)[["status"]] %in% "fail"))
})

test_that("changing only the preset says so when it normalises again", {
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  legacy <- nacho_thresholds(preset = "legacy")
  expect_message(
    normalise(GSE74821, outliers_thresholds = legacy),
    "thresholds preset"
  )
})

test_that("a panel without POS_E gives NA for PCL and LoD, not a failure", {
  no_pos_e <- GSE74821[nacho_probes(GSE74821)[["Name"]] != "POS_E(0.5)", ]
  res <- suppressMessages(normalise(no_pos_e, normalisation_method = "GEO"))
  expect_true(all(is.na(nacho_qc(res)[["PCL"]])))
  expect_true(all(is.na(nacho_qc(res)[["LoD"]])))
  expect_false(anyNA(nacho_qc(res)[["status"]]))
})

test_that("normalise() returns a nacho object", {
  x <- suppressMessages(normalise(salmon_nacho, normalisation_method = "GLM"))
  expect_true(S7::S7_inherits(x, NACHO:::nacho))
  expect_identical(x@settings$normalisation_method, "GLM")
})

test_that("GLM keeps the geometric mean of the kept negatives as Negative_factor", {
  x <- suppressMessages(normalise(GSE74821, normalisation_method = "GLM"))
  probes <- nacho_probes(x)
  kept <- probes$CodeClass == "Negative" & !probes$is_excluded
  expect_equal(
    nacho_samples(x)$Negative_factor,
    unname(exp(colMeans(log(pmax(nacho_counts(x)[kept, , drop = FALSE], 1)))))
  )
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
    nacho_qc(x)$status,
    NACHO:::qc_table(x@samples, tight, x@rcc_type, "IDFILE")$status
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

test_that("normalise() accepts open bounds and refuses closed infinite ones", {
  open_bounds <- salmon_nacho@thresholds
  open_bounds$LoD <- -Inf
  open_bounds$House_factor <- c(1 / 11, Inf)
  x <- suppressMessages(normalise(
    salmon_nacho,
    outliers_thresholds = open_bounds
  ))
  expect_identical(x@thresholds, open_bounds)
  bad <- salmon_nacho@thresholds
  bad$LoD <- Inf
  expect_error(
    normalise(salmon_nacho, outliers_thresholds = bad),
    class = "nacho_error_bad_argument"
  )
  bad <- salmon_nacho@thresholds
  bad$BD <- c(Inf, Inf)
  expect_error(
    normalise(salmon_nacho, outliers_thresholds = bad),
    class = "nacho_error_bad_argument"
  )
})

test_that("an open bound stops flagging the samples beyond it", {
  open_bounds <- utils::modifyList(
    nacho_thresholds(),
    list(
      BD = c(-Inf, Inf),
      FoV = 0,
      LoD = -Inf,
      PCL = 0,
      Positive_factor = c(-Inf, Inf),
      House_factor = c(-Inf, Inf)
    )
  )
  flags <- function(thresholds) {
    x <- suppressMessages(normalise(
      GSE74821,
      outliers_thresholds = thresholds
    ))
    nacho_qc(x)$status %in% "fail"
  }
  qc <- nacho_qc(GSE74821)
  expect_false(any(flags(open_bounds)))

  house <- open_bounds
  house$House_factor <- c(1, 1)
  expect_true(any(flags(house)))
  house$House_factor <- c(1, Inf)
  expect_identical(flags(house), qc$House_factor < 1)
  house$House_factor <- c(-Inf, 1)
  expect_identical(flags(house), qc$House_factor > 1)

  lod <- open_bounds
  lod$LoD <- stats::median(qc$LoD)
  expect_true(any(flags(lod)))
  lod$LoD <- -Inf
  expect_false(any(flags(lod)))
})

test_that("exclude_outliers() drops every flagged sample and normalises the rest", {
  tight <- plexset_nacho@thresholds
  tight$Positive_factor <- c(0.9, 1.1)
  flagged <- suppressMessages(normalise(
    plexset_nacho,
    outliers_thresholds = tight
  ))
  n_flagged <- sum(nacho_qc(flagged)$status %in% "fail")
  skip_if(
    n_flagged == 0 || n_flagged == ncol(flagged),
    "Tight thresholds flag no sample or every sample."
  )
  kept <- suppressMessages(exclude_outliers(flagged))
  expect_identical(ncol(kept), ncol(flagged) - n_flagged)
  expect_false(any(
    nacho_samples(kept)$IDFILE %in%
      nacho_samples(flagged)$IDFILE[nacho_qc(flagged)$status %in% "fail"]
  ))
  expect_identical(kept@thresholds, tight)
})

test_that("exclude_outliers() refuses to drop every sample", {
  flagged <- plexset_nacho
  flagged@thresholds$BD <- c(0, 0)
  expect_error(exclude_outliers(flagged), class = "nacho_error_bad_argument")
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
  flagged@samples$BD[1] <- 100
  expect_no_warning(suppressMessages(exclude_outliers(flagged)))
})

test_that("normalise() changes the background and keeps it in the settings", {
  x <- suppressMessages(normalise(
    GSE74821,
    background = "mean_2sd",
    background_mode = "threshold"
  ))
  expect_identical(x@settings$background, "mean_2sd")
  expect_identical(x@settings$background_mode, "threshold")
  negatives <- nacho_counts(x)[
    nacho_probes(x)$CodeClass == "Negative" & !nacho_probes(x)$is_excluded,
  ]
  expect_equal(
    nacho_samples(x)$Background,
    unname(colMeans(negatives) + 2 * apply(negatives, 2, stats::sd))
  )
  m <- nacho_counts(x, normalised = TRUE)
  expect_false(isTRUE(all.equal(m, round(m))))
})

test_that("normalise() refuses an unknown background", {
  expect_error(
    normalise(GSE74821, background = "mode"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    normalise(GSE74821, background_mode = "floor"),
    class = "nacho_error_bad_argument"
  )
})

test_that("the default is no background, and Background is NA", {
  x <- suppressMessages(normalise(GSE74821, background = "none"))
  expect_true(all(is.na(nacho_samples(x)$Background)))
  expect_identical(x@settings$background, "none")
})

test_that("RUVg normalisation stores W and the k it used", {
  x <- suppressMessages(normalise(
    GSE74821,
    normalisation_method = "RUVg",
    ruv_k = 2
  ))
  samples <- nacho_samples(x)
  expect_true(all(c("W_1", "W_2") %in% names(samples)))
  expect_false("House_factor" %in% names(samples))
  expect_identical(x@settings$ruv_k, 2L)
  expect_false(anyNA(nacho_counts(x, normalised = TRUE)[
    nacho_probes(x)$CodeClass == "Endogenous",
  ]))
  expect_true(all(nacho_counts(x, normalised = TRUE) >= 0, na.rm = TRUE))
})

test_that("RUVg without ruv_k uses the suggested k and says so", {
  withr::local_options(nacho.quiet = NULL, rlib_message_verbosity = NULL)
  expect_message(
    x <- normalise(GSE74821, normalisation_method = "RUVg"),
    "suggest_ruv_k"
  )
  expect_identical(
    x@settings$ruv_k,
    suggest_ruv_k(GSE74821)$k[suggest_ruv_k(GSE74821)$suggested]
  )
})

test_that("going back to GEO drops the W columns", {
  x <- suppressMessages(normalise(
    GSE74821,
    normalisation_method = "RUVg",
    ruv_k = 1
  ))
  y <- suppressMessages(normalise(x, normalisation_method = "GEO"))
  expect_false("W_1" %in% names(nacho_samples(y)))
})

test_that("ruv_k must be a whole number", {
  expect_error(
    normalise(GSE74821, normalisation_method = "RUVg", ruv_k = 1.5),
    class = "nacho_error_bad_argument"
  )
})

test_that("ruv_k = 0 normalises without W columns and can be normalised again", {
  x <- suppressMessages(normalise(
    GSE74821,
    normalisation_method = "RUVg",
    ruv_k = 0
  ))
  expect_identical(x@settings$ruv_k, 0L)
  expect_false(any(grepl("^W_", names(nacho_samples(x)))))
  y <- suppressMessages(normalise(x, n_comp = 3))
  expect_identical(y@settings$ruv_k, 0L)
})

test_that("RUVg corrects only endogenous and housekeeping probes", {
  geo <- suppressMessages(normalise(
    GSE74821,
    normalisation_method = "GEO",
    housekeeping_norm = FALSE
  ))
  ruv <- suppressMessages(normalise(
    GSE74821,
    normalisation_method = "RUVg",
    ruv_k = 2
  ))
  probes <- nacho_probes(ruv)
  controls <- probes$CodeClass %in% c("Positive", "Negative")
  expect_equal(
    nacho_counts(ruv, normalised = TRUE)[controls, ],
    nacho_counts(geo, normalised = TRUE)[controls, ]
  )
  input <- NACHO:::ruv_input(
    ruv@counts,
    ruv@probes,
    ruv@samples,
    ruv@settings,
    probes$Name[probes$is_housekeeping]
  )
  fit <- NACHO:::ruvg(input$log_expr, input$controls, 2)
  expect_equal(
    unname(nacho_counts(ruv, normalised = TRUE)[input$rows, ]),
    unname(pmax(2^t(fit$corrected) - 1, 0))
  )
})
