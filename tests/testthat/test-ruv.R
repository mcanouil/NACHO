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
    class = "nacho_error_bad_argument"
  )
})

test_that("rle_iqr() matches a hand computation", {
  log_expr <- matrix(c(1, 2, 3, 2, 4, 6, 3, 3, 3), nrow = 3, byrow = TRUE)
  rle <- sweep(log_expr, 2, apply(log_expr, 2, stats::median))
  expect_equal(NACHO:::rle_iqr(log_expr), mean(apply(rle, 1, stats::IQR)))
})
