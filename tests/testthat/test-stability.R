hand_expr <- matrix(
  c(
    1,
    2,
    1,
    2,
    3,
    4,
    3,
    4,
    2
  ),
  ncol = 3,
  byrow = TRUE,
  dimnames = list(c("s1", "s2", "s3"), c("g1", "g2", "g3"))
)

test_that("geNorm M matches a hand computation", {
  spread <- sqrt(7 / 3)
  expect_equal(
    NACHO:::genorm_m(hand_expr),
    c(g1 = spread / 2, g2 = spread / 2, g3 = spread)
  )
})

test_that("geNorm drops the least stable gene first and reports V", {
  result <- NACHO:::genorm_ranking(hand_expr)
  expect_identical(result$ranking, c("g1", "g2", "g3"))
  expect_identical(result$pairwise_v$pair, "2/3")
  expect_equal(result$pairwise_v$V, stats::sd(c(1 / 6, -1 / 2, 1 / 2)))
})

test_that("geNorm refuses fewer than three genes", {
  expect_error(
    NACHO:::genorm_ranking(hand_expr[, 1:2]),
    class = "nacho_error_bad_argument"
  )
})

test_that("geNorm matches NormqPCR", {
  skip_if_not_installed("NormqPCR")
  counts <- nacho_counts(GSE74821)
  rows <- nacho_probes(GSE74821)$CodeClass %in% c("Housekeeping", "Endogenous")
  log_expr <- t(log2(pmax(counts[rows, ][1:12, ], 1)))
  expect_equal(
    NACHO:::genorm_m(log_expr),
    NormqPCR::stabMeasureM(log_expr, log = TRUE),
    tolerance = 1e-10
  )
  reference <- NormqPCR::selectHKs(
    log_expr,
    method = "geNorm",
    minNrHKs = 2,
    log = TRUE,
    Symbols = colnames(log_expr),
    trace = FALSE
  )
  result <- NACHO:::genorm_ranking(log_expr)
  expect_identical(result$ranking, unname(reference$ranking))
  expect_equal(
    result$pairwise_v$V,
    unname(reference$variation),
    tolerance = 1e-10
  )
  expect_identical(result$pairwise_v$pair, names(reference$variation))
})
