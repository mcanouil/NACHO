test_that("default", {
  null <- capture.output(out <- print(GSE74821))
  expect_null(out)
})

test_that("show_legend to TRUE", {
  null <- capture.output(
    out <- print(GSE74821, colour = "CartridgeID", size = 0.5, show_legend = TRUE)
  )
  expect_null(out)
})
