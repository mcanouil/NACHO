test_that("a valid nacho object builds", {
  x <- toy_nacho()
  expect_true(S7::S7_inherits(x, NACHO:::nacho))
  expect_s3_class(x, "NACHO::nacho")
  expect_false(inherits(x, "nacho"))
})

test_that("the validator refuses mismatched dimensions", {
  x <- toy_nacho()
  expect_error(x@normalised <- x@normalised[-1, ], "same dimensions")
})

test_that("the validator refuses duplicated or misaligned sample ids", {
  x <- toy_nacho()
  samples <- x@samples
  samples$IDFILE[2] <- samples$IDFILE[1]
  expect_error(x@samples <- samples, "must match")
  samples <- x@samples[c(2, 1, 3, 4), ]
  expect_error(x@samples <- samples, "must match")
})

test_that("the validator refuses probes that do not match the counts", {
  x <- toy_nacho()
  probes <- x@probes
  probes$Name[1] <- "OTHER"
  expect_error(x@probes <- probes, "must match")
})

test_that("the validator refuses insane thresholds", {
  x <- toy_nacho()
  thresholds <- x@thresholds
  thresholds$BD <- c(2, 1)
  expect_error(x@thresholds <- thresholds, "BD")
  thresholds <- x@thresholds
  thresholds$PCL <- 2
  expect_error(x@thresholds <- thresholds, "PCL")
  thresholds <- x@thresholds
  thresholds$LoD <- NULL
  expect_error(x@thresholds <- thresholds, "LoD")
})

test_that("the validator refuses an unknown RCC type", {
  x <- toy_nacho()
  expect_error(x@rcc_type <- "n2", "rcc_type")
})

test_that("the validator refuses PCA scores whose row names do not match the sample ids", {
  x <- toy_nacho()
  pca <- x@pca
  rownames(pca$scores) <- rev(rownames(pca$scores))
  expect_error(x@pca <- pca, "pca\\$scores")
})

test_that("compute_outliers() matches the NACHO 2 rules", {
  samples <- data.frame(
    BD = c(1, 3, 1, 1),
    FoV = c(100, 100, 50, 100),
    PCL = c(1, 1, 1, 0.5),
    LoD = c(5, 5, 5, 5),
    Positive_factor = c(1, 1, 1, 1),
    House_factor = c(1, 1, 1, NA)
  )
  thresholds <- NACHO:::default_thresholds()
  expect_identical(
    NACHO:::compute_outliers(samples, thresholds, "n1"),
    c(FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(
    NACHO:::compute_outliers(samples, thresholds, "n8"),
    c(FALSE, TRUE, TRUE, FALSE)
  )
})

test_that("compute_pca() returns sample scores and caps the components", {
  counts <- matrix(
    c(1:12, 12:1, rep(5L, 12)),
    nrow = 12,
    dimnames = list(letters[1:12], c("a", "b", "c"))
  )
  expect_warning(
    pca <- NACHO:::compute_pca(counts, 5L),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(dim(pca$scores), c(3L, 2L))
  expect_identical(colnames(pca$scores), c("PC01", "PC02"))
  expect_identical(rownames(pca$scores), c("a", "b", "c"))
  expected <- stats::prcomp(t(log(counts + 1)))
  expect_equal(abs(unname(pca$scores)), abs(unname(expected$x[, 1:2])))
  expect_named(
    pca$importance,
    c(
      "PC",
      "Standard deviation",
      "Proportion of Variance",
      "Cumulative Proportion"
    )
  )
})

test_that("compute_pca() matches prcomp() whichever dimension is smaller", {
  withr::local_seed(42)
  for (shape in list(c(40L, 15L), c(15L, 40L))) {
    counts <- matrix(
      stats::rpois(prod(shape), 200),
      nrow = shape[1],
      dimnames = list(
        paste0("p", seq_len(shape[1])),
        paste0("s", seq_len(shape[2]))
      )
    )
    pca <- NACHO:::compute_pca(counts, 4L)
    expected <- stats::prcomp(t(log(counts + 1)))
    importance <- summary(expected)[["importance"]][, 1:4]
    expect_equal(
      abs(pca$scores),
      abs(expected$x[, 1:4]),
      tolerance = 1e-8,
      ignore_attr = TRUE
    )
    expect_identical(rownames(pca$scores), colnames(counts))
    expect_equal(
      as.matrix(pca$importance[, -1]),
      t(importance),
      tolerance = 1e-8,
      ignore_attr = TRUE
    )
  }
})

test_that("compute_pca() handles a single sample", {
  counts <- matrix(1:3, ncol = 1, dimnames = list(c("a", "b", "c"), "s1"))
  expect_warning(
    pca <- NACHO:::compute_pca(counts, 2L),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(dim(pca$scores), c(1L, 0L))
  expect_identical(nrow(pca$importance), 0L)
})

test_that("compute_pca() caps components by the number of probes too", {
  counts <- matrix(
    1:4,
    nrow = 1,
    dimnames = list("a", c("s1", "s2", "s3", "s4"))
  )
  expect_warning(
    pca <- NACHO:::compute_pca(counts, 2L),
    class = "nacho_warning_n_comp_reduced"
  )
  expect_identical(dim(pca$scores), c(4L, 1L))
  expect_identical(nrow(pca$importance), 1L)
})
