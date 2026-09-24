housekeeping_mean <- function(intercept) {
  geometric_housekeeping(
    data = data.table::data.table(
      Name = paste0("HK", 1:5),
      CodeClass = "Housekeeping",
      Count = c(5, 69, 10, 108, 4846)
    ),
    positive_factor = 1,
    intercept = intercept,
    housekeeping_genes = paste0("HK", 1:5)
  )
}

test_that("corrected counts between 0 and 1 are floored at 1", {
  expect_equal(housekeeping_mean(5 - 1e-12), housekeeping_mean(5))
})

test_that("geometric mean does not decrease as the intercept decreases", {
  means <- vapply(
    X = c(5.1, 5, 5 - 1e-12, 4.9, 4.5, 4),
    FUN = housekeeping_mean,
    FUN.VALUE = numeric(1)
  )
  expect_true(all(diff(means) >= 0))
})
