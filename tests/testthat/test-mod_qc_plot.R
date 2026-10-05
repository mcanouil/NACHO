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
