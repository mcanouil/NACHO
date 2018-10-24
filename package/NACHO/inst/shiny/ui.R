ui <- shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::img(src = "Nacho_logo.png", height = 150)
    ),
    shiny::column(
      width = 9,
      shiny::tabsetPanel(
        id = "maintabs",
        shiny::tabPanel(title = "QC Metrics", value = "met"),
        shiny::tabPanel(title = "Control Genes", value = "cg"),
        shiny::tabPanel(title = "QC Visuals", value = "vis"),
        shiny::tabPanel(title = "Normalisation Factors", value = "norm"),
        shiny::tabPanel(title = "Outlier Table", shiny::dataTableOutput("outlier_table"), value = "ot"),
        shiny::tabPanel(title = "About", value = "about")
      ),
      shiny::uiOutput("subtab")
    )
  ),
  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::uiOutput(outputId = "interfaceA"),
      shiny::uiOutput(outputId = "interfaceB"),
      shiny::uiOutput(outputId = "interfaceC"),
      shiny::uiOutput(outputId = "interfaceD"),
      shiny::uiOutput(outputId = "interfaceE"),
      shiny::uiOutput(outputId = "interfaceF"),
      shiny::uiOutput(outputId = "interfaceG"),
      shiny::tabsetPanel(
        id = "down",
        shiny::tabPanel(
          title = "Download",
          shiny::numericInput(inputId = "w", label = "Width:", width = 70, value = 10),
          shiny::numericInput(inputId = "h", label = "Height:", width = 70, value = 10),
          shiny::textInput(inputId = "name", label = "Plot name:"),
          shiny::actionButton(inputId = "do", label = "Download")
        )
      )
    ),
    shiny::column(
      width = 9,
      shiny::plotOutput(outputId = "all", width = "1280px", height = "720px")
    )
  )
)
