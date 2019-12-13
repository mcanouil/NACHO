panelInputUI <- function(id, label, ..., sidebar = NULL) {
  ns <- NS(id)

  contents <- list(...)
  contents_grid <- switch(as.character(length(contents)),
    "1" = {
      list(
        fluidRow(column(width = 12, align = "center", contents[[1]]))
      )
    },
    "2"  = {
      list(
        fluidRow(
          column(width = 6, align = "center", contents[[1]]),
          column(width = 6, align = "center", contents[[2]])
        )
      )
    },
    "3" = {
      list(
        fluidRow(
          column(width = 6, align = "center", contents[[1]]),
          column(width = 6, align = "center", contents[[2]])
        ),
        fluidRow(
          column(width = 12, align = "center", contents[[3]])
        )
      )
    },
    "4" = {
      list(
        fluidRow(
          column(width = 6, align = "center", contents[[1]]),
          column(width = 6, align = "center", contents[[2]])
        ),
        fluidRow(
          column(width = 6, align = "center", contents[[3]]),
          column(width = 6, align = "center", contents[[4]])
        )
      )
    },
    list(fluidRow())
  )

  if (is.null(sidebar)) {
    tabPanel(label, value = ns("tab"), contents_grid)
  } else {
    tabPanel(label, value = ns("tab"),
      sidebarLayout(
        sidebarPanel(width = 3,
          div(align = "center", sidebar)
        ),
        mainPanel(width = 9, contents_grid)
      )
    )
  }
}

plotInputUI <- function(label = NULL, ...) {
  id <- tolower(gsub('\\b(\\pL)\\pL|.', '\\U\\1', label, perl = TRUE))
  ns <- NS(id)
  list(
    fluidRow(
        h4(label, align = "center",
        dropdownButton(
          uiOutput(ns("plot_ui")),
          circle = TRUE,
          status = "danger",
          size = "sm",
          icon = icon("gear"),
          width = "800px",
          inline = TRUE,
          tooltip = tooltipOptions(title = "Click to see inputs!"),
          ...
        )
      )
    ),
    fluidRow(
      column(width = 12, align = "center", plotOutput(ns("server"), height = "350px"))
    )
  )
}

plotInput <- function(id, nacho) {
  callModule(id = id, function(input, output, session) {
    ns <- session$ns
    font_size <- 80
    output$plot_ui <- renderUI({
      fluidRow(
        column(width = 6,
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
             column(12, align = "center",
              selectInput(ns("group_colour"), span("Grouping Variable", helpText("(Colour)")),
                selected  = isolate(input$group_colour) %||% "CartridgeID",
                choices = c(
                  "CartridgeID",
                  "Date",
                  "ID",
                  "ScannerID",
                  "StagePosition"
                )
              )
            )
          ),
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(4, align = "center",
              radioButtons(ns("show_levels"), span("Show Levels", helpText("(Legend)")),
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = isolate(input$show_levels) %||% TRUE,
                inline = TRUE
              )
            ),
            column(4, align = "center",
              radioButtons(ns("show_outliers"), span("Show Outliers", helpText("(Point)")),
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = isolate(input$show_outliers) %||% TRUE,
                inline = TRUE
              )
            ),
            column(4, align = "center",
              radioButtons(ns("show_outliers_labels"), span("Outliers' Label", helpText("(Text)")),
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = isolate(input$show_outliers_labels) %||% FALSE,
                inline = TRUE
              )
            )
          ),
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(6, align = "center",
              sliderInput(ns("point_size"), span("Point Size", helpText("(mm)")),
                value = isolate(input$point_size) %||% 2,
                min = 1, max = 4
              )
            ),
            column(6, align = "center",
              sliderInput(ns("outliers_point_size"), span("Outliers Point Size", helpText("(Factor x Point Size)")),
                value = isolate(input$outliers_point_size) %||% 1.2,
                min = 1, max = 2
              )
            )
          )
        ),
        column(width = 6,
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(12, align = "center",
              numericInput(ns("font_size"), span("Font Size", helpText("(pt)")),
                value = isolate(input$font_size) %||% 16
              )
            )
          ),
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(4, align = "center",
              numericInput(ns("plot_width"), span("Width", helpText("(cm)")),
                value = isolate(input$plot_width) %||% 16
              )
            ),
            column(4, align = "center",
              numericInput(ns("plot_height"), span("Height", helpText("(cm)")),
                value = isolate(input$plot_height) %||% 12
              )
            ),
            column(4, align = "center",
              numericInput(ns("plot_dpi"), span("DPI", helpText("(Default: 120)")),
                value = isolate(input$plot_dpi) %||% 120
              )
            )
          ),
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(12, align = "center", style = 'padding-top: 2em;',
              downloadButton(ns("plot_download"), label = "Download")
            )
          )
        )
      )
    })

    output$server <- renderPlot({
      autoplot_values <- c(
        "bd" = "BD",
        "fov" = "FoV",
        "pcl" = "PC",
        "lod" = "LoD",
        "pp" = "Positive",
        "np" = "Negative",
        "hgp" = "Housekeeping",
        "cpe" = "PN",
        "acvbd" = "ACBD",
        "acvmc" = "ACMC",
        "pca" = "PCA",
        "pcai" = "PCAi",
        "pfvbt" = "PFNF",
        "hgf" = "HF",
        "nr" = "NORM"
      )
      x_metrics <- unname(autoplot_values[id])
      autoplot(
        x = x_metrics,
        object = nacho,
        colour = input[["group_colour"]] %||% "CartridgeID",
        size = input[["point_size"]] %||% 2,
        show_legend = as.logical(input[["show_levels"]] %||% TRUE)
      ) +
        theme_minimal(base_size = input[["font_size"]] %||% 16)
    })
  })
}
