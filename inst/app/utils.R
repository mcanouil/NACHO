panelInputUI <- function(id, label, ..., sidebar = NULL) {
  ns <- shiny::NS(id)

  contents <- list(...)

  contents_grid <- switch(as.character(length(contents)),
    "1" = {
      list(
        shiny::fluidRow(shiny::column(width = 12, align = "center", contents[[1]]))
      )
    },
    "2"  = {
      list(
        shiny::fluidRow(
          shiny::column(width = 6, align = "center", contents[[1]]),
          shiny::column(width = 6, align = "center", contents[[2]])
        )
      )
    },
    "3" = {
      list(
        shiny::fluidRow(
          shiny::column(width = 6, align = "center", contents[[1]]),
          shiny::column(width = 6, align = "center", contents[[2]])
        ),
        shiny::fluidRow(style = "padding-top: 1em;",
          shiny::column(width = 12, align = "center", contents[[3]])
        )
      )
    },
    "4" = {
      list(
        shiny::fluidRow(
          shiny::column(width = 6, align = "center", contents[[1]]),
          shiny::column(width = 6, align = "center", contents[[2]])
        ),
        shiny::fluidRow(style = "padding-top: 1em;",
          shiny::column(width = 6, align = "center", contents[[3]]),
          shiny::column(width = 6, align = "center", contents[[4]])
        )
      )
    },
    list(shiny::fluidRow())
  )

  if (is.null(sidebar)) {
    shiny::tabPanel(label, value = ns("tab"), contents_grid)
  } else {
    shiny::tabPanel(label, value = ns("tab"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3, shiny::tags$div(align = "center", sidebar)),
        shiny::mainPanel(width = 9, contents_grid)
      )
    )
  }
}

card <- function(title, body) {
  shiny::tags$div(class = paste0("card border-dark"),
    shiny::tags$div(class = "card-header", align = "center", title),
    shiny::tags$div(class = "card-body", align = "center", body)
  )
}

plotInputUI <- function(label = NULL, ...) {
  id <- tolower(gsub('\\b(\\pL)\\pL|.', '\\U\\1', label, perl = TRUE))
  ns <- shiny::NS(id)
  card(
    title = {
      shiny::tags$h4(label, align = "center",
        shinyWidgets::dropdownButton(
          shiny::uiOutput(ns("plot_ui")),
          circle = TRUE,
          status = "danger",
          size = "sm",
          icon = shiny::icon("gear"),
          width = "800px",
          inline = TRUE,
          tooltip = shinyWidgets::tooltipOptions(title = "Click to see inputs!"),
          ...
        )
      )
    },
    body = { shiny::plotOutput(ns("plot"), height = "350px") }
  )
}

plotInput <- function(id, nacho) {
  shiny::callModule(id = id, function(input, output, session) {
    ns <- session$ns
    font_size <- 80
    output$plot_ui <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(width = 6,
          shiny::fluidRow(style = paste0("font-size: ", font_size, "%;"),
             shiny::column(12, align = "center",
              shiny::selectInput(ns("group_colour"), shiny::tags$span("Grouping Variable", shiny::helpText("(Colour)")),
                selected  = shiny::isolate(input$group_colour) %||% "CartridgeID",
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
          shiny::fluidRow(style = paste0("font-size: ", font_size, "%;"),
            shiny::column(6, align = "center",
              shiny::radioButtons(ns("show_levels"), shiny::tags$span("Show Levels", shiny::helpText("(Legend)")),
                choiceNames = list("No", "Yes"),
                choiceValues = list(FALSE, TRUE),
                selected = shiny::isolate(input$show_levels) %||% TRUE,
                inline = TRUE
              )
            ),
            shiny::column(6, align = "center",
              shiny::radioButtons(ns("show_outliers"), shiny::tags$span("Show Outliers", shiny::helpText("(Point)")),
                choiceNames = list("No", "Yes"),
                choiceValues = list(FALSE, TRUE),
                selected = shiny::isolate(input$show_outliers) %||% TRUE,
                inline = TRUE
              )
            )
          ),
          shiny::fluidRow(style = paste0("font-size: ", font_size, "%;"),
            shiny::column(6, align = "center",
              shiny::radioButtons(ns("show_outliers_labels"), shiny::tags$span("Outliers' Label", shiny::helpText("(Text)")),
                choiceNames = list("No", "Yes"),
                choiceValues = list(FALSE, TRUE),
                selected = shiny::isolate(input$show_outliers_labels) %||% FALSE,
                inline = TRUE
              )
            ),
            shiny::column(6, align = "center",
              shiny::numericInput(ns("outliers_point_size"), shiny::tags$span("Outliers Point Size", shiny::helpText("(Factor x Point Size)")),
                value = shiny::isolate(input$outliers_point_size) %||% 1.5,
                min = 1, max = 3, step = 0.1
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(12, align = "center", shiny::uiOutput(ns("outliers_labels")))
          )
        ),
        shiny::column(width = 6,
          shiny::fluidRow(style = paste0("font-size: ", font_size, "%;"),
            shiny::column(6, align = "center",
              shiny::numericInput(ns("font_size"), shiny::tags$span("Font Size", shiny::helpText("(pt)")),
                value = shiny::isolate(input$font_size) %||% 16
              )
            ),
            shiny::column(6, align = "center",
              shiny::numericInput(ns("point_size"), shiny::tags$span("Point Size", shiny::helpText("(mm)")),
                value = shiny::isolate(input$point_size) %||% 2,
                min = 0, max = 4, step = 0.5
              )
            )
          ),
          shiny::fluidRow(style = paste0("font-size: ", font_size, "%;"),
            shiny::column(4, align = "center",
              shiny::numericInput(ns("plot_width"), shiny::tags$span("Width", shiny::helpText("(cm)")),
                value = shiny::isolate(input$plot_width) %||% 16
              )
            ),
            shiny::column(4, align = "center",
              shiny::numericInput(ns("plot_height"), shiny::tags$span("Height", shiny::helpText("(cm)")),
                value = shiny::isolate(input$plot_height) %||% 12
              )
            ),
            shiny::column(4, align = "center",
              shiny::numericInput(ns("plot_dpi"), shiny::tags$span("DPI", shiny::helpText("(Default: 120)")),
                value = shiny::isolate(input$plot_dpi) %||% 120
              )
            )
          ),
          shiny::fluidRow(style = paste0("font-size: ", font_size, "%;"),
            shiny::column(12, align = "center", style = 'padding-top: 2em;',
              shiny::downloadButton(ns("plot_download"), label = "Download")
            )
          )
        )
      )
    })

    output$outliers_labels <- shiny::renderUI({
      if (shiny::req(input[["show_outliers_labels"]])) {
        shiny::selectInput(ns("outliers_labels"), NULL, choices = colnames(nacho$nacho), selected = nacho$access)
      }
    })

    plot <- shiny::reactive({
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
      NACHO::autoplot(
        x = x_metrics,
        object = nacho,
        colour = input[["group_colour"]] %||% "CartridgeID",
        size = input[["point_size"]] %||% 2,
        show_legend = as.logical(input[["show_levels"]] %||% TRUE),
        show_outliers = as.logical(input[["show_outliers"]] %||% TRUE),
        outliers_factor = input[["outliers_point_size"]] %||% 2,
        outliers_labels = if (as.logical(input[["show_outliers_labels"]] %||% FALSE)) input[["outliers_labels"]] else NULL
      ) +
        ggplot2::theme_minimal(base_size = input[["font_size"]] %||% 16) +
        {
          if (x_metrics %in% c("NORM", "PN")) {
            ggplot2::theme(
              panel.grid.major.x = ggplot2::element_blank(),
              panel.grid.minor.x = ggplot2::element_blank()
            )
          }
        }
    })

    output$plot <- shiny::renderPlot({ plot() })

    output$plot_download <- shiny::downloadHandler(
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
