test_that("sliders follow the metrics the object has", {
  expect_identical(
    NACHO:::threshold_metrics(GSE74821),
    c(
      "BD",
      "FoV",
      "PCL",
      "LoD",
      "Positive_factor",
      "House_factor",
      "Housekeeping_detected"
    )
  )
  expect_false(any(
    c("PCL", "LoD", "House_factor") %in%
      NACHO:::threshold_metrics(plexset_nacho)
  ))
  ruv <- normalise(GSE74821, normalisation_method = "RUVg", ruv_k = 1)
  expect_false("House_factor" %in% NACHO:::threshold_metrics(ruv))
})

test_that("slider ranges cover the values and the finite limits", {
  expect_identical(
    NACHO:::threshold_range(c(0.2, 1.7), c(0.05, 2.25)),
    c(0, 2.5)
  )
  expect_identical(NACHO:::threshold_range(c(NA, Inf), c(-Inf, Inf)), c(0, 1))
  expect_identical(NACHO:::threshold_range(c(-3, -1), c(-Inf, 0)), c(-3, 0))
})

test_that("open bounds survive the sliders", {
  range <- c(-3, 0)
  original <- c(-Inf, 0)
  expect_identical(NACHO:::limits_to_slider(original, range), c(-3, 0))
  expect_identical(
    NACHO:::slider_to_limits(c(-3, -0.5), range, original),
    c(-Inf, -0.5)
  )
  expect_identical(
    NACHO:::slider_to_limits(c(-2, 0), range, original),
    c(-2, 0)
  )
  expect_identical(NACHO:::slider_to_limits(75, c(0, 100), 75), 75)
  expect_identical(NACHO:::slider_to_limits(0, c(0, 1), -Inf), -Inf)
})

test_that("normalisation choices follow the panel", {
  expect_identical(
    NACHO:::normalisation_choices(GSE74821),
    c("GEO", "GLM", "RUVg")
  )
  mirna <- suppressWarnings(mirna_fixture())
  expect_identical(
    NACHO:::normalisation_choices(mirna),
    c(
      "GEO",
      "GLM",
      "RUVg",
      "stable_mirna",
      "total_mirna",
      "spike_in",
      "ligation"
    )
  )
})

test_that("the module starts from the object's thresholds and debounces", {
  shiny::testServer(
    NACHO:::mod_thresholds_server,
    args = list(data = shiny::reactiveVal(GSE74821)),
    {
      session$flushReact()
      expect_identical(input$preset, NULL)
      thresholds <- session$returned$thresholds
      session$elapse(600)
      expect_identical(thresholds(), GSE74821@thresholds)
      session$setInputs(FoV = 99.9)
      session$elapse(600)
      expect_identical(thresholds()$FoV, 99.9)
      expect_identical(thresholds()$preset, GSE74821@thresholds$preset)
    }
  )
})

test_that("changing the preset resets the limits", {
  shiny::testServer(
    NACHO:::mod_thresholds_server,
    args = list(data = shiny::reactiveVal(GSE74821)),
    {
      session$setInputs(preset = "nsolver", instrument = "max")
      session$setInputs(preset = "legacy", instrument = "max")
      session$elapse(600)
      expect_identical(
        session$returned$thresholds()[c("BD", "Positive_factor")],
        nacho_thresholds("max", "legacy")[c("BD", "Positive_factor")]
      )
    }
  )
})

test_that("settings carry ruv_k only for RUVg", {
  shiny::testServer(
    NACHO:::mod_thresholds_server,
    args = list(data = shiny::reactiveVal(GSE74821)),
    {
      session$setInputs(
        method = "GLM",
        ruv_k = 2,
        background = "none",
        background_mode = "threshold"
      )
      expect_null(session$returned$settings()$ruv_k)
      session$setInputs(method = "RUVg")
      expect_identical(session$returned$settings()$ruv_k, 2L)
    }
  )
})

test_that("start-up inputs do not reset the object's own thresholds", {
  x <- GSE74821
  x@thresholds <- utils::modifyList(
    nacho_thresholds("flex", "legacy"),
    list(FoV = 80)
  )
  shiny::testServer(
    NACHO:::mod_thresholds_server,
    args = list(data = shiny::reactiveVal(x)),
    {
      session$setInputs(preset = "nsolver", instrument = "max")
      session$elapse(600)
      session$setInputs(preset = "legacy", instrument = "flex")
      session$elapse(600)
      expect_identical(session$returned$thresholds(), x@thresholds)
    }
  )
})

test_that("open bounds survive the module", {
  x <- suppressWarnings(mirna_fixture())
  neg_range <- NACHO:::threshold_range(
    x@samples$Ligation_NEG,
    x@thresholds$Ligation_NEG
  )
  haem_range <- NACHO:::threshold_range(
    x@samples$Haemolysis,
    x@thresholds$Haemolysis
  )
  shiny::testServer(
    NACHO:::mod_thresholds_server,
    args = list(data = shiny::reactiveVal(x)),
    {
      session$setInputs(
        Ligation_NEG = c(neg_range[1], -1),
        Haemolysis = c(haem_range[1], 5),
        FoV = 90
      )
      session$elapse(600)
      limits <- session$returned$thresholds()
      expect_identical(limits$Ligation_NEG, c(-Inf, -1))
      expect_identical(limits$Haemolysis, c(-Inf, 5))
      expect_identical(limits$FoV, 90)
    }
  )
})

test_that("slider steps put the current limits on the grid", {
  step <- NACHO:::slider_step("BD", c(0, 2.5), c(0.05, 2.25))
  expect_equal(0.05 / step, round(0.05 / step))
  expect_equal(2.25 / step, round(2.25 / step))
  expect_identical(NACHO:::slider_step("Housekeeping_detected", c(0, 12), 3), 1)
  expect_identical(NACHO:::slider_step("Ligation_order", c(0, 1), 1), 1)
  expect_identical(NACHO:::slider_step("FoV", c(0, 100), 75), 1)
  expect_gt(NACHO:::slider_step("LoD", c(0, 10), c(-Inf, Inf)), 0)
})
