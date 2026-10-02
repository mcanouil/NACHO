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
