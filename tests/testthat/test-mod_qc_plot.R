test_that("every app plot type is a known plot type", {
  types <- unlist(NACHO:::app_plot_types, use.names = FALSE)
  expect_true(all(types %in% names(NACHO:::nacho_plot_registry)))
  expect_false(anyDuplicated(types) > 0)
  expect_true(all(types %in% names(NACHO:::app_plot_titles)))
})

test_that("app_plot() muffles unavailable metrics only", {
  expect_no_warning(
    NACHO:::app_plot(
      plexset_nacho,
      "PCL",
      list(colour = "CartridgeID"),
      dark = FALSE
    )
  )
})

test_that("the module draws its plot and follows dark mode", {
  dark <- shiny::reactiveVal(FALSE)
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      type = "BD",
      dark = dark
    ),
    {
      session$setInputs(colour = "CartridgeID")
      expect_s3_class(session$returned(), "ggplot")
      expect_identical(output$plot$alt, NACHO:::plot_alt_texts[["BD"]])
      dark(TRUE)
      session$flushReact()
      theme <- ggplot2::complete_theme(session$returned()$theme)
      expect_identical(
        ggplot2::calc_element("plot.background", theme)$fill,
        "#111821"
      )
    }
  )
})

test_that("an unknown colour column falls back to CartridgeID", {
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      type = "BD",
      dark = shiny::reactive(FALSE)
    ),
    {
      session$setInputs(colour = "nope")
      plot <- session$returned()
      point_layers <- Filter(
        function(layer) inherits(layer$geom, "GeomPoint"),
        plot$layers
      )
      mappings <- c(list(plot$mapping), lapply(point_layers, `[[`, "mapping"))
      colours <- unlist(lapply(mappings, function(mapping) {
        if (is.null(mapping$colour)) NULL else rlang::as_label(mapping$colour)
      }))
      expect_true("CartridgeID" %in% colours)
    }
  )
})

test_that("plots that ignore colour have no colour control", {
  expect_match(
    as.character(NACHO:::mod_qc_plot_ui("BD")),
    "Colour by",
    fixed = TRUE
  )
  for (type in c("Stability", "PCBatch")) {
    expect_no_match(
      as.character(NACHO:::mod_qc_plot_ui(type)),
      "Colour by",
      fixed = TRUE
    )
  }
})

test_that("each card has a labelled options button and a summary", {
  html <- htmltools::renderTags(NACHO:::mod_qc_plot_ui("BD", "BD"))$html
  expect_match(
    html,
    'aria-label="Display options for Binding density"',
    fixed = TRUE
  )
  expect_match(html, "full-screen", fixed = TRUE)
  expect_match(html, 'id="BD-summary"', fixed = TRUE)
})

test_that("plot_summary() gives counts in one sentence", {
  x <- flagged_gse()
  expect_match(
    NACHO:::plot_summary(x),
    "^12 samples on 1 cartridges?; [1-9] flagged: GSM"
  )
  expect_identical(
    NACHO:::plot_summary(GSE74821),
    "48 samples on 4 cartridges; none flagged."
  )
})

test_that("the plot downloads as PNG at the chosen size", {
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      type = "BD",
      dark = shiny::reactive(FALSE)
    ),
    {
      session$setInputs(colour = "CartridgeID", width = 10, height = 8)
      path <- output$download
      expect_identical(unname(tools::file_ext(path)), "png")
      expect_gt(file.size(path), 1000)
    }
  )
})

test_that("plot_summary() lists at most three flagged samples", {
  x <- NACHO::GSE74821
  thresholds <- x@thresholds
  thresholds[["FoV"]] <- 99.9
  x <- NACHO::normalise(x, outliers_thresholds = thresholds)
  flagged <- sum(nacho_qc(x)[["status"]] %in% "fail")
  expect_gt(flagged, 4)
  summary <- NACHO:::plot_summary(x)
  expect_match(summary, sprintf("; %d flagged: ", flagged), fixed = TRUE)
  expect_match(summary, sprintf(" and %d more\\.$", flagged - 3))
  expect_equal(lengths(regmatches(summary, gregexpr("GSM", summary))), 3)
})

test_that("the summary counts flagged samples exactly", {
  x <- flagged_gse()
  flagged <- sum(nacho_qc(x)[["status"]] %in% "fail")
  expect_match(
    NACHO:::plot_summary(x),
    sprintf("; %d flagged: ", flagged),
    fixed = TRUE
  )
})

test_that("a cleared download size falls back to 16 by 12 cm", {
  skip_if_not_installed("png")
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      type = "BD",
      dark = shiny::reactive(FALSE)
    ),
    {
      session$setInputs(colour = "CartridgeID", width = NA, height = NA)
      path <- output$download
      dims <- dim(png::readPNG(path))
      expect_equal(dims[2] / dims[1], 16 / 12, tolerance = 0.01)
    }
  )
})

test_that("the download background is the paper colour", {
  skip_if_not_installed("png")
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = list(
      object = shiny::reactiveVal(GSE74821),
      type = "BD",
      dark = shiny::reactive(FALSE)
    ),
    {
      session$setInputs(colour = "CartridgeID", width = 10, height = 8)
      image <- png::readPNG(output$download)
      corner <- grDevices::rgb(image[1, 1, 1], image[1, 1, 2], image[1, 1, 3])
      expect_identical(
        toupper(corner),
        toupper(NACHO:::plot_colours(FALSE)[["paper"]])
      )
    }
  )
})

test_that("the display options reach the plot", {
  shiny::testServer(
    NACHO:::mod_qc_plot_server,
    args = list(
      object = shiny::reactiveVal(flagged_gse()),
      type = "FoV",
      dark = shiny::reactive(FALSE)
    ),
    {
      session$setInputs(
        colour = "CartridgeID",
        show_legend = FALSE,
        size = 3,
        labels = "CartridgeID"
      )
      built <- ggplot2::ggplot_build(session$returned())
      sizes <- unlist(lapply(built$data, function(d) d$size))
      expect_true(3 %in% sizes)
      expect_true(any(vapply(
        built$data,
        function(d) "label" %in% names(d),
        logical(1)
      )))
      expect_identical(
        session$returned()$guides$guides[["colour"]],
        "none"
      )
    }
  )
})

test_that("Stability keeps every option except colour", {
  html <- as.character(NACHO:::mod_qc_plot_ui("Stability"))
  expect_no_match(html, "Colour by", fixed = TRUE)
  for (label in c(
    "Show the legend",
    "Label flagged samples with",
    "Point size",
    "Download PNG"
  )) {
    expect_match(html, label, fixed = TRUE)
  }
})
