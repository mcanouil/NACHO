ssheet <- qcdata_ssheet[[2]]
ui <-fluidPage(
  fluidRow(column(3,
                  img(src = "Nacho_logo.png", height = 150)),
           column(9,
                  tabsetPanel(id = "maintabs",
                                tabPanel("QC Metrics", value = "met"),
                                tabPanel("Control Genes",value="cg"),
                                tabPanel("QC Visuals",value="vis"),
                                tabPanel("Normalisation Factors",value="norm"),
                                tabPanel("Outlier Table",dataTableOutput("outlier_table"), value="ot"),
                                tabPanel("About")),
                  uiOutput("subtab"))),
  fluidRow(column(3,
                  uiOutput("interfaceA"),
                  uiOutput("interfaceB"),
                  uiOutput("interfaceC"),
                  uiOutput("interfaceD"),
                  uiOutput("interfaceE"),
                  uiOutput("interfaceF"),
                  uiOutput("interfaceG"),
                  tabsetPanel(id="down",tabPanel("X"),
                              tabPanel("Download",
                                       numericInput("w","Width:", width = 70, value = 10),
                                       numericInput("h","Height:",width = 70, value = 10),
                                       textInput("name","Plot name:"),
                                       actionButton("do", "Download")))
                  
                  ),
           column(9,
                  plotOutput("all"),
                  plotOutput("pcimp")))
  
)