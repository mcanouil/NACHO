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

test_that("NormFinder matches NormqPCR with and without groups", {
  skip_if_not_installed("NormqPCR")
  counts <- nacho_counts(GSE74821)
  rows <- nacho_probes(GSE74821)$CodeClass %in% c("Housekeeping", "Endogenous")
  log_expr <- t(log2(pmax(counts[rows, ][1:12, ], 1)))
  group <- factor(rep(c("a", "b"), length.out = nrow(log_expr)))
  expect_equal(
    NACHO:::normfinder_rho(log_expr, group),
    NormqPCR::stabMeasureRho(log_expr, group = group, log = TRUE),
    tolerance = 1e-10
  )
  one_group <- suppressWarnings(NormqPCR::stabMeasureRho(
    log_expr,
    group = factor(rep("a", nrow(log_expr))),
    log = TRUE
  ))
  expect_equal(
    NACHO:::normfinder_rho(log_expr),
    sqrt(pmax(one_group, 0)),
    tolerance = 1e-10
  )
})

test_that("NormFinder refuses fewer than three genes and one-sample groups", {
  expect_error(
    NACHO:::normfinder_rho(hand_expr[, 1:2]),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    NACHO:::normfinder_rho(hand_expr, factor(c("a", "b", "b"))),
    regexp = "two samples",
    class = "nacho_error_bad_argument"
  )
})

test_that("housekeeping_stability() ranks the housekeeping genes", {
  result <- housekeeping_stability(GSE74821)
  ranking <- result$ranking
  housekeeping <- nacho_probes(GSE74821)$Name[
    nacho_probes(GSE74821)$CodeClass == "Housekeeping"
  ]
  expect_setequal(ranking$Name, housekeeping)
  expect_identical(ranking$geNorm_rank, seq_len(nrow(ranking)))
  expect_true(all(ranking$NormFinder_rho >= 0))
  expect_identical(nrow(result$pairwise_v), nrow(ranking) - 2L)
  expect_false("group_p_value" %in% names(ranking))
})

test_that("housekeeping_stability() tests genes against a group", {
  x <- GSE74821
  x@samples$arm <- rep(c("a", "b"), length.out = ncol(x))
  ranking <- housekeeping_stability(x, group = "arm")$ranking
  expect_true(all(ranking$group_p_value >= 0 & ranking$group_p_value <= 1))
  expect_equal(
    ranking$group_p_adjusted,
    stats::p.adjust(ranking$group_p_value, "BH")
  )
})

test_that("housekeeping_stability() refuses too few genes or bad arguments", {
  expect_error(
    housekeeping_stability(GSE74821, genes = nacho_probes(GSE74821)$Name[1:2]),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    housekeeping_stability(GSE74821, genes = "nope"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    housekeeping_stability(GSE74821, group = "nope"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    housekeeping_stability(GSE74821, min_detection = 1.5),
    class = "nacho_error_bad_argument"
  )
})

test_that("housekeeping_stability() refuses a group with one level", {
  x <- GSE74821
  x@samples$arm <- "a"
  expect_error(
    housekeeping_stability(x, group = "arm"),
    class = "nacho_error_bad_argument"
  )
})

test_that("housekeeping_stability() refuses a group with missing values", {
  x <- GSE74821
  x@samples$arm <- rep(c("a", "b"), length.out = ncol(x))
  x@samples$arm[1] <- NA
  expect_error(
    housekeeping_stability(x, group = "arm"),
    class = "nacho_error_bad_argument"
  )
})

test_that("genes with missing counts are left out of the candidates", {
  x <- GSE74821
  housekeeping <- nacho_probes(x)$Name[
    nacho_probes(x)$CodeClass == "Housekeeping"
  ]
  x@counts[housekeeping[1], 1] <- NA_integer_
  expect_false(housekeeping[1] %in% housekeeping_stability(x)$ranking$Name)
})

test_that("duplicated gene names are ranked once", {
  housekeeping <- nacho_probes(GSE74821)$Name[
    nacho_probes(GSE74821)$CodeClass == "Housekeeping"
  ]
  expect_identical(
    housekeeping_stability(GSE74821, genes = c(housekeeping, housekeeping)),
    housekeeping_stability(GSE74821, genes = housekeeping)
  )
})

test_that("too few genes left after dropping some is an error", {
  x <- GSE74821
  housekeeping <- nacho_probes(x)$Name[
    nacho_probes(x)$CodeClass == "Housekeeping"
  ]
  x@counts[housekeeping[-(1:2)], 1] <- NA_integer_
  expect_error(
    housekeeping_stability(x),
    class = "nacho_error_bad_argument"
  )
})

test_that("predicted housekeeping genes are the five most stable by geNorm", {
  x <- suppressMessages(normalise(GSE74821, housekeeping_predict = TRUE))
  predicted <- x@settings$housekeeping_genes
  expect_length(predicted, 5)
  candidates <- nacho_probes(GSE74821)$Name[
    grepl("Endogenous|Housekeeping", nacho_probes(GSE74821)$CodeClass) &
      nacho_probes(GSE74821)$detection_rate >= 0.9
  ]
  expected <- housekeeping_stability(GSE74821, genes = candidates)$ranking$Name[
    1:5
  ]
  expect_identical(predicted, expected)
})
