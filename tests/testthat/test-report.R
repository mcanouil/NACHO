grDevices::pdf(NULL)
null_device <- grDevices::dev.cur()

test_that("default", {
  expect_output(NACHO:::report_markdown(GSE74821), "RCC Summary")
  utils::capture.output(
    res <- withVisible(NACHO:::report_markdown(GSE74821))
  )
  expect_false(res[["visible"]])
  expect_identical(res[["value"]], GSE74821)
})

test_that("missing object", {
  expect_error(NACHO:::report_markdown(), class = "nacho_error_bad_object")
})

test_that("show_legend to TRUE", {
  utils::capture.output(
    res <- NACHO:::report_markdown(
      GSE74821,
      colour = "CartridgeID",
      size = 0.5,
      show_legend = TRUE
    )
  )
  expect_true(S7::S7_inherits(res))
})

test_that("not a nacho object", {
  expect_error(
    NACHO:::report_markdown(list(nacho = data.frame())),
    class = "nacho_error_bad_object"
  )
})

test_that("numeric column for colour", {
  x <- GSE74821
  x@thresholds[["FoV"]] <- 95
  x <- check_outliers(x)
  x@samples[["channel_count"]] <- as.numeric(x@samples[["channel_count"]])
  expect_output(
    NACHO:::report_markdown(x, colour = "channel_count"),
    "Outliers"
  )
})

grDevices::dev.off(null_device)
