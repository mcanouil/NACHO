test_that("log-10 transform keeps infinite values in both directions", {
  transformation <- NACHO:::transform_log10_infinite()
  values <- c(-Inf, 1, 10, 100, Inf)
  expect_identical(transformation$transform(values), c(-Inf, 0, 1, 2, Inf))
  expect_identical(
    transformation$inverse(transformation$transform(values)),
    values
  )
})
