ruv_expr <- function() {
  gse <- NACHO::GSE74821
  counts <- nacho_counts(gse)
  rows <- grepl("Endogenous|Housekeeping", nacho_probes(gse)$CodeClass)
  log_expr <- t(log2(counts[rows, ] + 1))
  controls <- colnames(log_expr) %in%
    nacho_probes(gse)$Name[
      nacho_probes(gse)$CodeClass == "Housekeeping"
    ]
  list(log_expr = log_expr, controls = controls)
}

test_that("ruvg() matches RUVSeq::RUVg() on log data", {
  skip_if_not_installed("RUVSeq")
  d <- ruv_expr()
  ours <- NACHO:::ruvg(d$log_expr, d$controls, k = 2)
  theirs <- RUVSeq::RUVg(t(d$log_expr), which(d$controls), k = 2, isLog = TRUE)
  expect_equal(unname(ours$W), unname(theirs$W), tolerance = 1e-10)
  expect_equal(
    unname(ours$corrected),
    unname(t(theirs$normalizedCounts)),
    tolerance = 1e-10
  )
  expect_identical(colnames(ours$W), c("W_1", "W_2"))
})

test_that("ruvg() with k = 0 changes nothing", {
  d <- ruv_expr()
  out <- NACHO:::ruvg(d$log_expr, d$controls, k = 0)
  expect_identical(out$corrected, d$log_expr)
  expect_identical(ncol(out$W), 0L)
})

test_that("ruvg() refuses missing control counts", {
  d <- ruv_expr()
  d$log_expr[1, which(d$controls)[1]] <- NA
  expect_error(
    NACHO:::ruvg(d$log_expr, d$controls, k = 1),
    class = "nacho_error_bad_argument",
    regexp = colnames(d$log_expr)[which(d$controls)[1]]
  )
})

test_that("ruvg() gives no factor when the controls are constant", {
  d <- ruv_expr()
  d$log_expr[, d$controls] <- 5
  out <- NACHO:::ruvg(d$log_expr, d$controls, k = 1)
  expect_identical(ncol(out$W), 0L)
  expect_identical(out$corrected, d$log_expr)
})

test_that("ruvg() refuses an empty set of controls", {
  d <- ruv_expr()
  expect_error(
    NACHO:::ruvg(d$log_expr, rep(FALSE, ncol(d$log_expr)), k = 1),
    class = "nacho_error_bad_argument",
    regexp = "control genes"
  )
})

test_that("rle_iqr() matches a hand computation", {
  log_expr <- matrix(c(1, 2, 3, 2, 4, 6, 3, 3, 3), nrow = 3, byrow = TRUE)
  rle <- sweep(log_expr, 2, apply(log_expr, 2, stats::median))
  expect_equal(NACHO:::rle_iqr(log_expr), mean(apply(rle, 1, stats::IQR)))
})

test_that("suggest_ruv_k() picks the smallest k close to the best RLE", {
  table <- suggest_ruv_k(GSE74821, max_k = 3)
  expect_identical(table$k, 0:3)
  expect_identical(sum(table$suggested), 1L)
  best <- min(table$rle_iqr)
  expect_identical(
    table$k[table$suggested],
    min(table$k[table$rle_iqr <= 1.05 * best])
  )
  expect_true(all(table$pc1_variance > 0 & table$pc1_variance <= 1))
})

test_that("suggest_ruv_k() caps k at what the data allow", {
  x <- suppressWarnings(GSE74821[, 1:3])
  expect_lte(max(suggest_ruv_k(x, max_k = 10)$k), 2L)
})
