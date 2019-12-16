library(shiny)
library(shinyWidgets)
library(rlang)
library(purrr)
library(ggplot2)
library(dplyr)
library(NACHO)

source("utils.R")

ui <- navbarPage(
  theme = "united-bootstrap.min.css",
  title = span(img(src = "nacho_hex.png", height = 18), "NACHO"),
  windowTitle = "NACHO",
  collapsible = TRUE,
  id = "main-menu",
  # footer = div(
  #   p(
  #     img(src = "www/nacho_hex.png", height = 18),
  #     "Full documentation on", icon("github"), "at",
  #     a(url = "https://mcanouil.github.io/NACHO", "mcanouil.github.io/NACHO")
  #   ),
  #   align = "center",
  #   style = "margin: 5px;"
  # ),
  panelInputUI("qc_metrics", "QC Metrics",
    sidebar = {
      list(
        h3("QC Thresholds"),
        radioButtons("qc_bd_metrics", span("Binding Density", actionLink("about_bd", NULL, icon = icon("info-circle"))),
          choiceNames = list(
            span("MAX/FLEX", helpText("(Default: 0.1 - 2.25)")),
            span("SPRINT", helpText("(Default: 0.1 - 1.8)"))
          ),
          choiceValues = list("MAX/FLEX", "SPRINT"),
          inline = TRUE
        ),
        sliderInput("qc_bd_thresh", NULL,
          min = 0.1, max = 2.25, value = c(0.1, 2.25)
        ),
        sliderInput("qc_fov_thresh", span("Field of View", actionLink("about_fov", NULL, icon = icon("info-circle")), helpText("(Default: 75)")),
          min = 50, max = 100, value = 75
        ),
        sliderInput("qc_pcl_thresh", span("Positive Control Linearity", actionLink("about_pcl", NULL, icon = icon("info-circle")), helpText("(Default: 0.95)")),
          min = 0.5, max = 1, value = 0.95
        ),
        sliderInput("qc_lod_thresh", span("Limit of Detection", actionLink("about_lod", NULL, icon = icon("info-circle")), helpText("(Default: 2)")),
          min = 0, max = 30, value = 2
        )
      )
    },
    plotInputUI("Binding Density"),
    plotInputUI("Field of View", right = TRUE),
    plotInputUI("Positive Control Linearity"),
    plotInputUI("Limit of Detection", right = TRUE)
  ),
  panelInputUI("qc_control", "Control Probes",
    sidebar = NULL,
    plotInputUI("Positive Probes"),
    plotInputUI("Negative Probes", right = TRUE),
    plotInputUI("Housekeeping Genes Probes"),
    plotInputUI("Control Probe Expression", right = TRUE)
  ),
  panelInputUI("qc_count", "Counts",
    sidebar = NULL,
    plotInputUI("Average Count vs. Binding Density"),
    plotInputUI("Average Count vs. Median Count", right = TRUE),
    plotInputUI("Principal Component Analysis"),
    plotInputUI("Principal Component Analysis Inertia", right = TRUE)
  ),
  panelInputUI("norm", "Normalisation",
    sidebar = {
      list(
        sliderInput("qc_pf_thresh", span("Positive Factor", helpText("(Default: 0.25 - 4)")),
          min = 0.25, max = 4, value = c(0.25, 4)
        ),
        sliderInput("qc_hgf_thresh", span("Housekeeping Genes Factor", helpText("(Default: 0.09 - 11)")),
          min = 0.09, max = 11, value = c(0.09, 11)
        )
      )
    },
    plotInputUI("Positive Factor vs. Negative Factor"),
    plotInputUI("Housekeeeping Genes Factor", right = TRUE),
    plotInputUI("Normalisation Result", right = TRUE)
  ),
  tabPanel("About", icon = icon("info"), value = "about-tab",
    p(includeMarkdown("www/about-nacho.md"))
  )
)

server <- function(input, output, session) {
  # ---------------------------------------- UI / SERVER
  # Global UI input
  observe({
    nacho_tmp <- nacho_custom()
    map(
      .x = c(
        "bd", "fov", "pcl", "lod",
        "pp", "np", "hgp", "cpe",
        "acvbd", "acvmc", "pca", "pcai",
        "pfvnf", "hgf", "nr"
      ),
      .f = ~ plotInput(.x, nacho_tmp)
    )
  })

  # QC metrics UI input
  ## Update UI with thresholds
  observe({
    switch(req(input$qc_bd_metrics),
      "MAX/FLEX" = {
        updateSliderInput(session, "qc_bd_thresh",
          max = 2.25,
          value = isolate(input$qc_bd_thresh)
        )
      },
      "SPRINT" = {
        updateSliderInput(session, "qc_bd_thresh",
          max = 1.8,
          value = isolate(input$qc_bd_thresh)
        )
      }
    )
    updateSliderInput(session, "qc_fov_thresh",
      value = isolate(input$qc_fov_thresh)
    )
    updateSliderInput(session, "qc_pcl_thresh",
      value = isolate(input$qc_pcl_thresh)
    )
    updateSliderInput(session, "qc_lod_thresh",
      value = isolate(input$qc_lod_thresh)
    )
  })

  ## Help for QC metrics
  map(
    .x = c("Binding Density", "Field of View", "Positive Control Linearity", "Limit of Detection"),
    .f = function(.x) {
      short_x <- tolower(gsub('\\b(\\pL)\\pL|.', '\\U\\1', .x, perl = TRUE))
      observeEvent(input[[paste0("about_", short_x)]], {
        showModal(modalDialog(
          title = .x,
          p(includeMarkdown(paste0("www/about-", short_x,".md"))),
          easyClose = TRUE
        ))
      })
    }
  )

  # ---------------------------------------- Input
  # Get nacho object and update thresholds
  nacho_custom <- reactive({
    nacho_object <- get(data(GSE74821, package = "NACHO"))
    nacho_object$outliers_thresholds[["BD"]] <- input$qc_bd_thresh %||%
      nacho_object$outliers_thresholds[["BD"]]
    nacho_object$outliers_thresholds[["FoV"]] <- input$qc_fov_thresh %||%
      nacho_object$outliers_thresholds[["FoV"]]
    nacho_object$outliers_thresholds[["LoD"]] <- input$qc_lod_thresh %||%
      nacho_object$outliers_thresholds[["LoD"]]
    nacho_object$outliers_thresholds[["PCL"]] <- input$qc_pcl_thresh %||%
      nacho_object$outliers_thresholds[["PCL"]]
    nacho_object$outliers_thresholds[["Positive_factor"]] <- input$qc_pf_thresh %||%
      nacho_object$outliers_thresholds[["Positive_factor"]]
    nacho_object$outliers_thresholds[["House_factor"]] <- input$qc_hgf_thresh %||%
      nacho_object$outliers_thresholds[["House_factor"]]
    nacho_object
  })

  # ---------------------------------------- Output
  outliers_list <- reactive({
    ot <- nacho_custom()$outliers_thresholds
    df <- distinct(
      nacho_custom()$nacho,
      sample_ID, CartridgeID, BD, FoV, PCL, LoD, MC, MedC,
      Positive_factor, House_factor
    )
    filter(df,
      BD < min(ot[["BD"]]) | BD > max(ot[["BD"]]) |
      FoV < ot[["FoV"]] |
      PCL < ot[["PCL"]] |
      LoD < ot[["LoD"]] |
      Positive_factor < min(ot[["Positive_factor"]]) | Positive_factor > max(ot[["Positive_factor"]]) |
      House_factor < min(ot[["House_factor"]]) | House_factor > max(ot[["House_factor"]])
    )
  })
  output[["outliers"]] <- renderTable({ outliers_list() })
  output[["outliers-thresholds"]] <- renderUI({
    ot <- lapply(nacho_custom()$outliers_thresholds, round, digits = 3)
    tags$div(
      tags$ul(
        tags$li(
          'Binding Density (', code("BD"), ') <', strong(min(ot[["BD"]])),
          'or Binding Density (', code("BD"), ') >', strong(max(ot[["BD"]]))
        ),
        tags$li('Field of View (', code("FoV"), ') <', strong(ot[["FoV"]])),
        tags$li('Positive Control Linearity (', code("PCL"), ') <', strong(min(ot[["PCL"]]))),
        tags$li('Limit of Detection (', code("LoD"), ') <', strong(min(ot[["LoD"]]))),
        tags$li(
          'Positive Normalisation Dactor (', code("Positive_factor"), ') <', strong(min(ot[["Positive_factor"]])),
          'or Positive Normalisation Dactor (', code("Positive_factor"), ') >', strong(max(ot[["Positive_factor"]]))),
        tags$li(
          'Housekeeping Normalisation Factor (', code("house_factor"), ') <', strong(min(ot[["House_factor"]])),
          'or Housekeeping Normalisation Dactor (', code("house_factor"), ') >', strong(max(ot[["House_factor"]]))
        )
      )
    )
  })
  observe({
    req(nrow(outliers_list()) != 0)
    insertTab("main-menu",
      tab = {
        tabPanel(title = "Outliers", value = "outliers-tab",
          card(title = h4("Outliers List"), list(uiOutput("outliers-thresholds"), tableOutput("outliers")))
        )
      },
      target = "about-tab",
      position = "before"
    )
  })
}

shinyApp(ui = ui, server = server)
