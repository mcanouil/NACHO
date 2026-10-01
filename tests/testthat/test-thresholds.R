test_that("nsolver thresholds follow the Bruker knowledge base", {
  x <- nacho_thresholds()
  expect_identical(x$preset, "nsolver")
  expect_identical(x$instrument, "max")
  expect_identical(x$BD, c(0.05, 2.25))
  expect_identical(x$FoV, 75)
  expect_identical(x$PCL, 0.95)
  expect_identical(x$LoD, 2)
  expect_identical(x$Positive_factor, c(0.3, 3))
  expect_identical(x$House_factor, c(0.1, 10))
  expect_identical(nacho_thresholds("sprint")$BD, c(0.1, 1.8))
  expect_identical(nacho_thresholds("pro")$BD, c(0.05, 2.25))
})

test_that("legacy thresholds reproduce NACHO 2 on every instrument", {
  for (instrument in c("max", "flex", "pro", "sprint")) {
    x <- nacho_thresholds(instrument, preset = "legacy")
    expect_identical(x$BD, c(0.1, 2.25), info = instrument)
    expect_identical(x$Positive_factor, c(1 / 4, 4), info = instrument)
    expect_identical(x$House_factor, c(1 / 11, 11), info = instrument)
  }
})

test_that("nacho_thresholds() refuses unknown instruments and presets", {
  expect_error(nacho_thresholds("nano"), class = "nacho_error_bad_argument")
  expect_error(
    nacho_thresholds(preset = "strict"),
    class = "nacho_error_bad_argument"
  )
})

test_that("detect_instrument() knows only the Digital Analyzer file version", {
  expect_identical(
    NACHO:::detect_instrument(
      data.frame(Header.header_FileVersion = c("1.7", "1.7"))
    ),
    "max"
  )
  expect_identical(
    NACHO:::detect_instrument(data.frame(Header.header_FileVersion = "2.0")),
    NA_character_
  )
  expect_identical(NACHO:::detect_instrument(data.frame(x = 1)), NA_character_)
})

test_that("an unknown instrument warns once and uses the MAX/FLEX values", {
  samples <- data.frame(Header.header_FileVersion = "2.0")
  expect_warning(
    x <- NACHO:::thresholds_for_samples(samples, NULL, "nsolver"),
    regexp = "instrument",
    class = "nacho_warning_instrument_unknown"
  )
  expect_identical(x$BD, c(0.05, 2.25))
  expect_no_warning(
    NACHO:::thresholds_for_samples(samples, "sprint", "nsolver")
  )
})

test_that("a NACHO 2 thresholds list points to nacho_thresholds()", {
  old <- nacho_thresholds()[c(
    "BD",
    "FoV",
    "LoD",
    "PCL",
    "Positive_factor",
    "House_factor"
  )]
  expect_error(
    normalise(GSE74821, outliers_thresholds = old),
    regexp = "nacho_thresholds",
    class = "nacho_error_bad_argument"
  )
})

test_that("the validator checks preset and instrument", {
  x <- nacho_thresholds()
  x$preset <- "strict"
  expect_match(NACHO:::validate_thresholds(x), "preset", all = FALSE)
  x <- nacho_thresholds()
  x$instrument <- NA_character_
  expect_length(NACHO:::validate_thresholds(x), 0)
})

test_that("the housekeeping detection threshold is a lower bound", {
  expect_identical(nacho_thresholds()$Housekeeping_detected, 3)
  expect_identical(
    nacho_thresholds(preset = "legacy")$Housekeeping_detected,
    0
  )
  x <- nacho_thresholds()
  x$Housekeeping_detected <- Inf
  expect_match(
    NACHO:::validate_thresholds(x),
    "Housekeeping_detected",
    all = FALSE
  )
  x$Housekeeping_detected <- c(1, 2)
  expect_match(
    NACHO:::validate_thresholds(x),
    "Housekeeping_detected",
    all = FALSE
  )
  x$Housekeeping_detected <- NULL
  expect_match(NACHO:::validate_thresholds(x), "lacks", all = FALSE)
})

test_that("ligation and haemolysis thresholds follow the preset", {
  nsolver <- nacho_thresholds("sprint")
  expect_identical(nsolver$Ligation_order, 1)
  expect_identical(nsolver$Ligation_R2, 0.95)
  expect_identical(nsolver$Ligation_NEG, c(-Inf, 0))
  expect_identical(nsolver$Haemolysis, c(-Inf, Inf))
  expect_identical(
    nacho_thresholds("sprint", haemolysis = TRUE)$Haemolysis,
    c(-Inf, 7)
  )
  legacy <- nacho_thresholds("sprint", preset = "legacy")
  expect_identical(legacy$Ligation_order, 0)
  expect_identical(legacy$Ligation_R2, 0)
  expect_identical(legacy$Ligation_NEG, c(-Inf, Inf))
  expect_error(
    nacho_thresholds(haemolysis = NA),
    class = "nacho_error_bad_argument"
  )
  expect_length(validate_thresholds(nsolver), 0)
  expect_length(validate_thresholds(nacho_thresholds(haemolysis = TRUE)), 0)
  nsolver$Ligation_R2 <- 2
  expect_match(validate_thresholds(nsolver), "Ligation_R2")
})

test_that("thresholds saved before the ligation elements still validate", {
  saved <- nacho_thresholds()
  saved[c(
    "Ligation_order",
    "Ligation_R2",
    "Ligation_NEG",
    "Haemolysis"
  )] <- NULL
  expect_length(validate_thresholds(saved), 0)
})
