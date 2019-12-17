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
        fluidRow(style = "padding-top: 1em;",
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
        fluidRow(style = "padding-top: 1em;",
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
        sidebarPanel(width = 3, tags$div(align = "center", sidebar)),
        mainPanel(width = 9, contents_grid)
      )
    )
  }
}

card <- function(title, body) {
  tags$div(class = paste0("card border-dark"),
    tags$div(class = "card-header", align = "center", title),
    tags$div(class = "card-body", align = "center", body)
  )
}

plotInputUI <- function(label = NULL, ...) {
  id <- tolower(gsub('\\b(\\pL)\\pL|.', '\\U\\1', label, perl = TRUE))
  ns <- NS(id)
  card(
    title = {
      tags$h4(label, align = "center",
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
    },
    body = { plotOutput(ns("plot"), height = "350px") }
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
              selectInput(ns("group_colour"), tags$span("Grouping Variable", helpText("(Colour)")),
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
            column(6, align = "center",
              radioButtons(ns("show_levels"), tags$span("Show Levels", helpText("(Legend)")),
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = isolate(input$show_levels) %||% TRUE,
                inline = TRUE
              )
            ),
            column(6, align = "center",
              radioButtons(ns("show_outliers"), tags$span("Show Outliers", helpText("(Point)")),
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = isolate(input$show_outliers) %||% TRUE,
                inline = TRUE
              )
            )
          ),
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(6, align = "center",
              radioButtons(ns("show_outliers_labels"), tags$span("Outliers' Label", helpText("(Text)")),
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = isolate(input$show_outliers_labels) %||% FALSE,
                inline = TRUE
              )
            ),
            column(6, align = "center",
              numericInput(ns("outliers_point_size"), tags$span("Outliers Point Size", helpText("(Factor x Point Size)")),
                value = isolate(input$outliers_point_size) %||% 1.5,
                min = 1, max = 3, step = 0.1
              )
            )
          ),
          fluidRow(
            column(12, align = "center", uiOutput(ns("outliers_labels")))
          )
        ),
        column(width = 6,
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(6, align = "center",
              numericInput(ns("font_size"), tags$span("Font Size", helpText("(pt)")),
                value = isolate(input$font_size) %||% 16
              )
            ),
            column(6, align = "center",
              numericInput(ns("point_size"), tags$span("Point Size", helpText("(mm)")),
                value = isolate(input$point_size) %||% 2,
                min = 1, max = 4, step = 0.5
              )
            )
          ),
          fluidRow(style = paste0("font-size: ", font_size, "%;"),
            column(4, align = "center",
              numericInput(ns("plot_width"), tags$span("Width", helpText("(cm)")),
                value = isolate(input$plot_width) %||% 16
              )
            ),
            column(4, align = "center",
              numericInput(ns("plot_height"), tags$span("Height", helpText("(cm)")),
                value = isolate(input$plot_height) %||% 12
              )
            ),
            column(4, align = "center",
              numericInput(ns("plot_dpi"), tags$span("DPI", helpText("(Default: 120)")),
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

    output$outliers_labels <- renderUI({
      if (req(input[["show_outliers_labels"]])) {
        selectInput(ns("outliers_labels"), NULL, choices = colnames(nacho$nacho), selected = nacho$access)
      }
    })

    plot <- reactive({
      autoplot_values <- c(
        "bd" = "BD",
        "fov" = "FoV",
        "pcl" = "PCL",
        "lod" = "LoD",
        "pp" = "Positive",
        "np" = "Negative",
        "hgp" = "Housekeeping",
        "cpe" = "PN",
        "acvbd" = "ACBD",
        "acvmc" = "ACMC",
        "pca" = "PCA",
        "pcai" = "PCAi",
        "pfvnf" = "PFNF",
        "hgf" = "HF",
        "nr" = "NORM"
      )
      x_metrics <- unname(autoplot_values[id])
      autoplot(
        x = x_metrics,
        object = nacho,
        colour = input[["group_colour"]] %||% "CartridgeID",
        size = input[["point_size"]] %||% 2,
        show_legend = as.logical(input[["show_levels"]] %||% TRUE),
        show_outliers = as.logical(input[["show_outliers"]] %||% TRUE),
        outliers_factor = input[["outliers_point_size"]] %||% 2,
        outliers_labels = if (as.logical(input[["show_outliers_labels"]] %||% FALSE)) input[["outliers_labels"]] else NULL
      ) +
        theme_minimal(base_size = input[["font_size"]] %||% 16) +
        {
          if (x_metrics %in% c("NORM", "PN")) {
            ggplot2::theme(
              panel.grid.major.x = ggplot2::element_blank(),
              panel.grid.minor.x = ggplot2::element_blank()
            )
          }
        }
    })

    output$plot <- renderPlot({ plot() })

    output$plot_download <- downloadHandler(
      filename = function() {
        autoplot_values <- c(
          "bd" = "BD",
          "fov" = "FoV",
          "pcl" = "PCL",
          "lod" = "LoD",
          "pp" = "Positive",
          "np" = "Negative",
          "hgp" = "Housekeeping",
          "cpe" = "PN",
          "acvbd" = "ACBD",
          "acvmc" = "ACMC",
          "pca" = "PCA",
          "pcai" = "PCAi",
          "pfvnf" = "PFNF",
          "hgf" = "HF",
          "nr" = "NORM"
        )
        x_metrics <- unname(autoplot_values[id])
        paste0(unname(autoplot_values[id]), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(
          filename = file, plot = plot(),
          width = input[["plot_width"]], height = input[["plot_height"]],
          units = "cm",
          dpi = input[["plot_dpi"]]
        )
      }
    )
  })
}
