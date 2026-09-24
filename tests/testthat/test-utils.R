test_that("log-10 transform keeps infinite values in both directions", {
  transformation <- NACHO:::transform_log10_infinite()
  values <- c(-Inf, 1, 10, 100, Inf)
  expect_identical(transformation$transform(values), c(-Inf, 0, 1, 2, Inf))
  expect_identical(
    transformation$inverse(transformation$transform(values)),
    values
  )
})

test_that("log-10 transform supports log tick guides", {
  plot <- autoplot(GSE74821, x = "HF") +
    ggplot2::guides(y = ggplot2::guide_axis_logticks())
  expect_no_error(ggplot2::ggplot_build(plot))
})
