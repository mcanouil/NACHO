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

test_that("the report leaves out open threshold bounds", {
  x <- toy_nacho(4L)
  thresholds <- x@thresholds
  thresholds$BD <- c(-Inf, 2.25)
  thresholds$House_factor <- c(1 / 11, Inf)
  thresholds$LoD <- -Inf
  x@thresholds <- thresholds
  output <- utils::capture.output(NACHO:::report_markdown(x))
  expect_false(any(grepl("Inf", output, fixed = TRUE)))
  expect_length(grep("Binding Density (BD)", output, fixed = TRUE), 1L)
  expect_length(grep("Limit of Detection (LoD) <", output, fixed = TRUE), 0L)
  expect_length(grep("(house_factor) <", output, fixed = TRUE), 1L)
  expect_length(grep("(house_factor) >", output, fixed = TRUE), 0L)
})

test_that("the settings list the housekeeping genes, or none", {
  housekeeping_line <- function(is_housekeeping) {
    toy <- toy_nacho(4L)
    probes <- toy@probes
    probes[["is_housekeeping"]] <- is_housekeeping
    toy@probes <- probes
    output <- utils::capture.output(NACHO:::report_markdown(toy))
    grep("Housekeeping genes available", output, value = TRUE)
  }
  expect_identical(
    housekeeping_line(FALSE),
    "  - Housekeeping genes available: none "
  )
  expect_identical(
    housekeeping_line(c(rep(FALSE, 8), TRUE, FALSE, FALSE)),
    "  - Housekeeping genes available: HK1 "
  )
  expect_identical(
    housekeeping_line(c(rep(FALSE, 8), TRUE, TRUE, FALSE)),
    "  - Housekeeping genes available: HK1 and GENE1 "
  )
})

grDevices::dev.off(null_device)
