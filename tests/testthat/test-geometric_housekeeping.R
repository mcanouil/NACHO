housekeeping_mean <- function(negative_factor) {
  NACHO:::housekeeping_geometric_means(
    matrix(c(5, 69, 10, 108, 4846), ncol = 1),
    negative_factor = negative_factor,
    positive_factor = 1
  )
}

test_that("corrected counts between 0 and 1 are floored at 1", {
  counts <- matrix(c(10.5, 100), ncol = 1)
  expect_equal(
    NACHO:::housekeeping_geometric_means(
      counts,
      negative_factor = 10,
      positive_factor = 1
    ),
    exp(mean(log(c(1, 90))))
  )
  expect_equal(housekeeping_mean(5 - 1e-12), housekeeping_mean(5))
})

test_that("only corrected counts below 1 are floored", {
  expect_equal(
    housekeeping_mean(4.5),
    exp(mean(log(c(1, 64.5, 5.5, 103.5, 4841.5))))
  )
})

test_that("geometric mean does not decrease as the negative factor decreases", {
  means <- vapply(
    X = c(5.1, 5, 5 - 1e-12, 4.9, 4.5, 4),
    FUN = housekeeping_mean,
    FUN.VALUE = numeric(1)
  )
  expect_true(all(diff(means) >= 0))
})
