grDevices::pdf(NULL)
null_device <- grDevices::dev.cur()

test_that("default", {
  utils::capture.output(res <- withVisible(print(GSE74821)))
  expect_false(res[["visible"]])
  expect_identical(res[["value"]], GSE74821)
})

test_that("missing object", {
  expect_error(print.nacho())
})

test_that("show_legend to TRUE", {
  expect_s3_class(
    print.nacho(
      GSE74821,
      colour = "CartridgeID",
      size = 0.5,
      show_legend = TRUE
    ),
    "nacho"
  )
})

test_that("wrong attribute", {
  attr(GSE74821, "RCC_type") <- "something_wrong"
  expect_error(print.nacho(GSE74821))
})

test_that("numeric column for colour", {
  GSE74821$outliers_thresholds$FoV <- 95
  GSE74821$nacho$channel_count <- as.numeric(GSE74821$nacho$channel_count)
  expect_s3_class(
    print.nacho(GSE74821, colour = "channel_count", echo = TRUE),
    "nacho"
  )
})

test_that("xaringan", {
  expect_s3_class(
    print.nacho(GSE74821, xaringan = TRUE, echo = TRUE),
    "nacho"
  )
})

grDevices::dev.off(null_device)
