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

test_that("the validator reports duplicated sample ids", {
  x <- toy_nacho()
  ids <- c("S01.RCC", "S01.RCC", "S03.RCC", "S04.RCC")
  samples <- x@samples
  samples$IDFILE <- ids
  counts <- x@counts
  colnames(counts) <- ids
  err <- expect_error(
    S7::set_props(
      x,
      samples = samples,
      counts = counts,
      normalised = counts * 1,
      pca = list()
    ),
    "Sample ids must be unique"
  )
  expect_match(conditionMessage(err), "S01.RCC", fixed = TRUE)
})

test_that("the validator lists at most three duplicated sample ids", {
  x <- toy_nacho(8L)
  ids <- rep(sprintf("S%02d.RCC", 1:4), each = 2)
  samples <- x@samples
  samples$IDFILE <- ids
  counts <- x@counts
  colnames(counts) <- ids
  err <- expect_error(
    S7::set_props(
      x,
      samples = samples,
      counts = counts,
      normalised = counts * 1,
      pca = list()
    ),
    "Sample ids must be unique"
  )
  expect_match(
    conditionMessage(err),
    "S01.RCC, S02.RCC, S03.RCC",
    fixed = TRUE
  )
  expect_no_match(conditionMessage(err), "S04.RCC", fixed = TRUE)
})

test_that("the validator refuses probes that do not match the counts", {
  x <- toy_nacho()
  probes <- x@probes
  probes$Name[1] <- "OTHER"
  expect_error(x@probes <- probes, "must match")
})

test_that("the validator reports duplicated probe names", {
  x <- toy_nacho()
  probes <- x@probes
  probes$Name[2] <- probes$Name[1]
  counts <- x@counts
  rownames(counts) <- probes$Name
  normalised <- x@normalised
  rownames(normalised) <- probes$Name
  expect_error(
    S7::set_props(
      x,
      probes = probes,
      counts = counts,
      normalised = normalised
    ),
    "Probe names must be unique"
  )
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

test_that("the validator accepts infinities that mean no bound", {
  x <- toy_nacho()
  thresholds <- x@thresholds
  thresholds$LoD <- -Inf
  thresholds$House_factor <- c(1 / 11, Inf)
  thresholds$BD <- c(-Inf, 2.25)
  thresholds$Positive_factor <- c(-Inf, Inf)
  x@thresholds <- thresholds
  expect_identical(x@thresholds, thresholds)
})

test_that("the validator refuses infinities that are not an open bound", {
  x <- toy_nacho()
  refused <- list(
    LoD = Inf,
    LoD = NaN,
    BD = c(Inf, Inf),
    BD = c(-Inf, -Inf),
    Positive_factor = c(NaN, 4),
    House_factor = c(11, 1 / 11)
  )
  for (k in seq_along(refused)) {
    name <- names(refused)[k]
    thresholds <- x@thresholds
    thresholds[[name]] <- refused[[k]]
    expect_error(x@thresholds <- thresholds, name)
  }
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

#' Flip each column so its largest absolute value is positive
#'
#' The reference implementation of the sign convention, used to check
#' `compute_pca()` from outside its own code.
fix_signs <- function(scores) {
  for (j in seq_len(ncol(scores))) {
    largest <- which.max(abs(scores[, j]))
    if (scores[largest, j] < 0) {
      scores[, j] <- -scores[, j]
    }
  }
  scores
}

test_that("compute_pca() gives each component a fixed sign", {
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
    expect_identical(unname(pca$scores), unname(fix_signs(pca$scores)))
  }
})

test_that("compute_pca() matches prcomp() up to the sign convention", {
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
    expect_equal(
      unname(pca$scores),
      unname(fix_signs(expected$x[, 1:4])),
      tolerance = 1e-8
    )
  }
})

test_that("compute_pca() gives consistent signs for a row-permuted matrix", {
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
    permuted <- counts[sample(nrow(counts)), , drop = FALSE]
    pca <- NACHO:::compute_pca(counts, 4L)
    pca_permuted <- NACHO:::compute_pca(permuted, 4L)
    expect_equal(pca_permuted$scores, pca$scores, tolerance = 1e-8)
  }
})

test_that("compute_pca() refuses a missing n_comp with an internal error", {
  counts <- matrix(
    1:20,
    nrow = 5,
    dimnames = list(paste0("p", 1:5), paste0("s", 1:4))
  )
  error <- expect_error(
    NACHO:::compute_pca(counts, NULL),
    class = "nacho_error_internal"
  )
  expect_match(conditionMessage(error), "NULL", fixed = TRUE)
  error <- expect_error(
    NACHO:::compute_pca(counts, -1L),
    class = "nacho_error_internal"
  )
  expect_match(conditionMessage(error), "at least 0, not -1.", fixed = TRUE)
})
