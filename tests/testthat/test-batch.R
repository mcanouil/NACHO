test_that("Cramér's V ignores unused factor levels", {
  expect_equal(
    NACHO:::cramers_v(
      factor(c("a", "a", "b", "b"), levels = c("a", "b", "c")),
      c("x", "x", "y", "y")
    ),
    1
  )
})

test_that("PlexSet lane metrics are tested once per lane", {
  samples <- nacho_samples(plexset_nacho)
  id <- plexset_nacho@settings[["id_colname"]]
  rows <- NACHO:::metric_rows(samples, "BD", plexset_nacho)
  expect_identical(
    sum(rows),
    length(unique(sub("_S[0-9]*$", "", samples[[id]])))
  )
  expect_lt(sum(rows), nrow(samples))
  expect_true(all(NACHO:::metric_rows(samples, "MC", plexset_nacho)))
  expect_true(all(NACHO:::metric_rows(nacho_samples(GSE74821), "BD", GSE74821)))
})

test_that("Cramér's V is 1 for full confounding and 0 for balance", {
  expect_equal(NACHO:::cramers_v(c(1, 1, 2, 2), c("x", "x", "y", "y")), 1)
  expect_equal(NACHO:::cramers_v(c(1, 2, 1, 2), c("x", "x", "y", "y")), 0)
  expect_identical(NACHO:::cramers_v(c(1, 2), c("x", "x")), NA_real_)
})

test_that("group_r2() matches a one-way ANOVA", {
  values <- c(1, 2, 3, 7, 8, 9)
  batch <- c("a", "a", "a", "b", "b", "b")
  expect_equal(
    NACHO:::group_r2(values, batch),
    summary(stats::lm(values ~ factor(batch)))$r.squared
  )
  expect_identical(NACHO:::group_r2(values, rep("a", 6)), NA_real_)
})

test_that("GSE270837 is fully confounded with cartridge", {
  x <- mirna_fixture()
  x@samples$status <- ifelse(
    grepl("Healthy", x@samples$title),
    "healthy",
    "patient"
  )
  result <- batch_diagnostics(x, group = "status")
  cartridge <- result$design[result$design$batch == "CartridgeID", ]
  expect_equal(cartridge$cramers_v, 1)
  expect_identical(cartridge$single_group_levels, 2L)
  expect_true(cartridge$confounded)
  date <- result$design[result$design$batch == "Date", ]
  expect_identical(date$n_levels, 1L)
  expect_true(is.na(date$cramers_v))
  expect_false(date$confounded)
  expect_identical(dim(result$crosstabs$CartridgeID), c(2L, 2L))
})

test_that("metric tests and PC R² cover each batch variable", {
  result <- batch_diagnostics(GSE74821)
  expect_null(result$design)
  expect_setequal(unique(result$metrics$batch), c("CartridgeID", "Date"))
  expect_true(all(
    result$metrics$p_value >= 0 & result$metrics$p_value <= 1,
    na.rm = TRUE
  ))
  expect_identical(
    nrow(result$pc_batch),
    ncol(GSE74821@pca$scores) * 2L
  )
  expect_true(all(
    result$pc_batch$r_squared >= 0 & result$pc_batch$r_squared <= 1,
    na.rm = TRUE
  ))
})

test_that("batch_diagnostics() checks its columns", {
  expect_error(
    batch_diagnostics(GSE74821, group = "nope"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    batch_diagnostics(GSE74821, batch = "nope"),
    class = "nacho_error_bad_argument"
  )
})

test_that("a one-sample cartridge does not make the design confounded", {
  x <- toy_nacho(6L)
  x@samples[["CartridgeID"]] <- c("a", "a", "a", "a", "b", "c")
  x@samples[["status"]] <- c("x", "y", "x", "y", "x", "y")
  result <- batch_diagnostics(x, group = "status", batch = "CartridgeID")
  expect_identical(result$design$single_group_levels, 0L)
  expect_false(result$design$confounded)
})

test_that("a one-level batch variable gives NA rows", {
  result <- batch_diagnostics(mirna_fixture(), batch = "Date")
  expect_gt(nrow(result$metrics), 0L)
  expect_true(all(is.na(result$metrics$statistic)))
  expect_true(all(is.na(result$metrics$p_value)))
  expect_gt(nrow(result$pc_batch), 0L)
  expect_true(all(is.na(result$pc_batch$r_squared)))
})

test_that("metrics and pc_batch are empty data frames when nothing is reported", {
  x <- toy_nacho(6L)
  x@pca[["scores"]] <- x@pca[["scores"]][, 0, drop = FALSE]
  x@samples <- x@samples[, c("IDFILE", "CartridgeID"), drop = FALSE]
  result <- batch_diagnostics(x, batch = "CartridgeID")
  expect_s3_class(result$metrics, "data.frame")
  expect_s3_class(result$pc_batch, "data.frame")
  expect_identical(nrow(result$metrics), 0L)
  expect_identical(nrow(result$pc_batch), 0L)
  expect_named(result$metrics, c("metric", "batch", "statistic", "p_value"))
  expect_named(result$pc_batch, c("PC", "batch", "r_squared"))
})
