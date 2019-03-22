#' visualise
#'
#' @inheritParams normalise
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(GEOquery)
#' library(NACHO)
#'
#' # Import data from GEO
#' gse <- GEOquery::getGEO(GEO = "GSE74821")
#' targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
#' GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
#' utils::untar(
#'   tarfile = paste0(tempdir(), "/GSE74821/GSE74821_RAW.tar"),
#'   exdir = paste0(tempdir(), "/GSE74821")
#' )
#' targets$IDFILE <- list.files(
#'   path = paste0(tempdir(), "/GSE74821"),
#'   pattern = ".RCC.gz$"
#' )
#' targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
#' utils::write.csv(
#'   x = targets,
#'   file = paste0(tempdir(), "/GSE74821/Samplesheet.csv")
#' )
#'
#'
#' # Read RCC files and format
#' nacho <- summarise(
#'    data_directory = paste0(tempdir(), "/GSE74821"),
#'    ssheet_csv = paste0(tempdir(), "/GSE74821/Samplesheet.csv"),
#'    id_colname = "IDFILE"
#' )
#' visualise(nacho)
#'
#'
#' # (re)Normalise data by removing outliers
#' nacho_norm <- normalise(
#'   nacho_object = nacho,
#'   remove_outliers = TRUE
#' )
#' visualise(nacho_norm)
#'
#'
#' # (re)Normalise data with "GLM" method and removing outliers
#' nacho_norm <- normalise(
#'   nacho_object = nacho,
#'   normalisation_method = "GLM"
#'   remove_outliers = TRUE
#' )
#' visualise(nacho_norm)
#'
#' }
#'
visualise <- function(nacho_object) {
  mandatory_fields <- c(
    "access",
    "housekeeping_genes",
    "housekeeping_predict",
    "housekeeping_norm",
    "normalisation_method",
    "remove_outliers",
    "n_comp",
    "data_directory",
    "pc_sum",
    "nacho",
    "outliers_thresholds",
    "raw_counts",
    "normalised_counts"
  )
  if (!interactive()) {
    stop('[NACHO] Must be run in an interactive R session!')
  }
  if (!all(mandatory_fields%in%names(nacho_object))) {
    stop(
      '[NACHO] Mandatory fields are missing in "', substitute(nacho_object), '"!\n',
      '  "summarise()" and/or "normalise()" must be called before "visualise()".'
    )
  }

  id_colname <- nacho_object[["access"]]
  housekeeping_genes <- nacho_object[["housekeeping_genes"]]
  housekeeping_norm <- nacho_object[["housekeeping_norm"]]
  pc_sum <- nacho_object[["pc_sum"]]
  nacho <- nacho_object[["nacho"]]
  save_path_default <- nacho_object[["data_directory"]]
  type_set <- attr(nacho_object, "RCC_type")
  outliers_env <- new.env()
  assign(x = "outliers_thresholds", value = nacho_object[["outliers_thresholds"]], envir = outliers_env)

  message(
    '[NACHO] Custom "outliers_thresholds" can be loaded for later use with:\n',
    '  outliers_thresholds <- readRDS("', tempdir(), '/outliers_thresholds.rds")'
  )

  shiny::addResourcePath("logo", system.file("help", "figures", package = "NACHO"))

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3,
          shiny::div(shiny::img(src = "logo/nacho_hex.png", height = 150), align = "center"),
          shiny::br(),
          shiny::tabsetPanel(
            id = "settings",
            selected = "Panel",
            shiny::tabPanel(
              title = "General",
              shiny::br(),
              shiny::numericInput(inputId = "font_size", label = "Font size:", value = 14),
              shiny::numericInput(inputId = "max_factors", label = "Max factors (legend):", value = 10)
            ),
            shiny::tabPanel(
              title = "Panel",
              shiny::br(),
              shiny::uiOutput(outputId = "interfaceA"),
              shiny::uiOutput(outputId = "interfaceB"),
              shiny::uiOutput(outputId = "interfaceC"),
              shiny::uiOutput(outputId = "interfaceD"),
              shiny::uiOutput(outputId = "interfaceE1"),
              shiny::uiOutput(outputId = "interfaceE2"),
              shiny::uiOutput(outputId = "interfaceF"),
              shiny::uiOutput(outputId = "interfaceG"),
              shiny::uiOutput(outputId = "interfaceH")
            ),
            shiny::tabPanel(
              title = "Download",
              shiny::br(),
              shiny::textInput(inputId = "name", label = "Plot name (with extension):"),
              shiny::textInput(inputId = "save_path", label = "Output directory:", value = save_path_default),
              shiny::numericInput(inputId = "w", label = "Width (inch):", value = 8),
              shiny::numericInput(inputId = "h", label = "Height (inch):", value = 6),
              shiny::numericInput(inputId = "dpi", label = "DPI:", value = 120),
              shiny::div(shiny::actionButton(inputId = "do", label = "Download"), align = "center")
            )
          )
        ),
        shiny::mainPanel(width = 9,
          shiny::tabsetPanel(
            id = "maintabs",
            shiny::tabPanel(title = "QC Metrics", value = "met"),
            shiny::tabPanel(title = "Control Genes", value = "cg"),
            shiny::tabPanel(title = "QC Visuals", value = "vis"),
            shiny::tabPanel(title = "Normalisation Factors", value = "norm"),
            shiny::tabPanel(title = "Outlier Table", value = "ot"),
            shiny::tabPanel(title = "About", value = "about")
          ),
          shiny::uiOutput("subtab"),
          shiny::dataTableOutput("outlier_table"),
          shiny::plotOutput(outputId = "all", width = "100%", height = "600px")
        )
      )
    ),
    server = function(input, output) {
      shiny::observeEvent(input$do, {
        file_ext <- function (x) {
            pos <- regexpr("\\.([[:alnum:]]+)$", x)
            ifelse(pos > -1L, substring(x, pos + 1L), "")
        }

        if (!dir.exists(input$save_path)) {
          dir.create(path = input$save_path)
        }
        clean_path <- normalizePath(input$save_path)
        ggplot2::ggsave(
          filename = paste0(
            clean_path, "/",
            input$name, if (file_ext(input$name)=="") ".png"
          ),
          width = input$w,
          height = input$h,
          units = "in",
          dpi = input$dpi
        )
        shiny::showNotification(paste0('Plot successfully saved to: "', clean_path, '"'))
      })

      # Render subtabs based on maintab
      output$subtab <- shiny::renderUI({
        shiny::req(input$maintabs!="ot")
        switch(
          EXPR = input$maintabs,
          "met" = {
            switch(
              EXPR = type_set,
              "n1" = {
                shiny::tabsetPanel(
                  id = "tabs",
                  shiny::tabPanel("Binding Density", value = "BD"),
                  shiny::tabPanel("Imaging", value = "FoV"),
                  shiny::tabPanel("Positive Control Linearity", value = "PC"),
                  shiny::tabPanel("Limit of Detection", value = "LoD")
                )
              },
              "n8" = {
                shiny::tabsetPanel(
                  id = "tabs",
                  shiny::tabPanel("Binding Density", value = "BD"),
                  shiny::tabPanel("Imaging", value = "FoV")
                )
              }
            )
          },
          "cg" = {
            tabslist <- unique(nacho[["CodeClass"]][-grep("Endogenous", nacho[["CodeClass"]])])
            tabslist <- setdiff(c(tabslist, "Control Probe Expression"), "Message")
            do.call(
              what = "tabsetPanel",
              c(
                id = "tabs",
                lapply(tabslist, function(i) {
                  shiny::tabPanel(title = i)
                })
              )
            )
          },
          "norm" = {
            if (housekeeping_norm & !is.null(housekeeping_genes)) {
              shiny::tabsetPanel(
                id = "tabs",
                shiny::tabPanel("Positive Factor vs. Background Threshold", value = "pfbt"),
                shiny::tabPanel("Housekeeping Factor", value = "hf"),
                shiny::tabPanel("Normalisation Result", value = "norm_res")
              )
            } else {
              shiny::tabsetPanel(
                id = "tabs",
                shiny::tabPanel("Positive Factor vs. Background Threshold", value = "pfbt"),
                shiny::tabPanel("Normalisation Result", value = "norm_res")
              )
            }
          },
          "vis" = {
            shiny::tabsetPanel(
              id = "tabs",
              shiny::tabPanel("Average Count vs. Binding Density", value = "MC-BD"),
              shiny::tabPanel("Average Count vs. Median Count", value = "MC-MedC"),
              shiny::tabPanel("Principal Component", value = "prin")
            )
          },
          "about" = {
            vignette_exists <- system.file("doc", "NACHO.Rmd", package = "NACHO")
            if (nchar(vignette_exists)==0) {
              p("The vignette was not build.")
            }

            p(
              shiny::br(),
              shiny::em("NACHO"), " (NanoString Quality Control Dashboard) is developed for NanoString nCounter data.",
              shiny::br(),
              "NanoString nCounter data is a mRNA or miRNA expression assay and works with fluorescent barcodes.",
              shiny::br(),
              "Each barcode is assigned a mRNA/miRNA, which can be counted after bonding with its target.",
              shiny::br(),
              "As a result each count of a specific barcode represents the presence of its target mRNA/miRNA.",
              shiny::br(),
              shiny::br(),
              shiny::em("NACHO"), ' is able to load, visualise and normalise the exported NanoString nCounter data and facilitates the user in performing a quality control.',
              shiny::br(),
              shiny::em("NACHO"), ' does this by visualising Quality Control metrics, expression of control genes, principal components and sample specific size factors in an interactive web application.',
              shiny::br(),
              'With the use of two functions, RCC files are summarised and visualised, namely: ', shiny::code("summarise()"), ' and ', shiny::code("visualise()"), '.',
              shiny::br(),
              'The ', shiny::code("summarise()"), ' function is used to preprocess the data.',
              shiny::br(),
              'The ', shiny::code("visualise()"), ' function initiates a RStudio Shiny-based dashboard that visualises all relevant QC plots.',
              shiny::br(),
              shiny::br(),
              shiny::em("NACHO"), ' also includes a function ', shiny::code("normalise()"), ', which calculates sample specific size factors and normalises the data.',
              shiny::br(),
              shiny::code("normalise()"), ' creates a list in which your settings, the raw counts and normalised counts are stored.'
            )
          }
        )
      })

      # Render interface options based on maintab
      output$interfaceA <- shiny::renderUI({
        shiny::req(input$maintabs)
        if (input$maintabs == "met") {
          shiny::selectInput(
            inputId = "attribute",
            label = "Select x-axis",
            choices = c(
              "CartridgeID",
              "Date",
              "ID",
              "ScannerID",
              "StagePosition"
            )
          )
        } else if (input$maintabs == "vis" | input$maintabs == "norm" | input$maintabs == "cg") {
          shiny::radioButtons(
            inputId = "colour_choice",
            label = "Select colour attributes:",
            choiceNames = c("RCC attributes", "Samplesheet attributes"),
            choiceValues = c(FALSE, TRUE)
          )
        }
      })

      output$interfaceB <- shiny::renderUI({
        shiny::req(input$maintabs)
        colour_variables <- c(id_colname, names(which(sapply(X = nacho, FUN = function(y) {
          unique_y <- unique(y)
          length(unique_y) <= 20 && length(unique_y) > 0 && any(!is.na(unique_y))
        }))))
        if (input$maintabs == "met") {
          shiny::selectInput(
            inputId = "meta",
            label = "Coloured by:",
            choices = colour_variables,
            selected = "CodeClass"
          )
        } else if (input$maintabs == "vis" | input$maintabs == "norm" | input$maintabs == "cg") {
           colour <- ifelse(is.null(input$colour_choice), TRUE, input$colour_choice)
          if (colour) {
            shiny::selectInput(
              inputId = "meta",
              label = "Coloured by:",
              choices = colour_variables,
              selected = "CodeClass"
            )
          } else {
            shiny::selectInput(
              inputId = "meta",
              label = "Coloured by:",
              choices = c(
                "CartridgeID",
                "Date",
                "ID",
                "ScannerID",
                "StagePosition"
              )
            )
          }
        }
      })

      output$interfaceC <- shiny::renderUI({
        shiny::req(input$maintabs%in%c("met", "vis"))
        if (input$maintabs == "met") {
          shiny::div(
            shiny::checkboxInput(
              inputId = "outlier",
              label = "View outliers",
              value = TRUE
            ),
            align = "center"
          )
        } else if (input$maintabs == "vis") {
          shiny::req(input$tabs == "prin")
          shiny::selectInput(
            inputId = "pcA_sel",
            label = "PC on x-axis:",
            choices = grep("^PC[0-9]+$", colnames(nacho), value = TRUE)
          )
        }
      })

      output$interfaceD <- shiny::renderUI({
        shiny::req(input$maintabs%in%c("met", "vis"))
        if (input$maintabs == "met") {
          shiny::div(
            shiny::checkboxInput(
              inputId = "outlab",
              label = "View outlier labels"
            ),
            align = "center"
          )
        } else if (input$maintabs == "vis") {
          shiny::req(input$tabs == "prin")
          shiny::selectInput(
            inputId = "pcB_sel",
            label = "PC on y-axis:",
            choices = grep("^PC[0-9]+$", colnames(nacho), value = TRUE),
            selected = grep("^PC[0-9]+$", colnames(nacho), value = TRUE)[2]
          )
        }
      })

      output$interfaceE1 <- shiny::renderUI({
        shiny::req(!input$maintabs %in% c("ot", "about"))
        shiny::sliderInput(
          inputId = "point_size",
          label = "Point size",
          min = 1,
          max = 5,
          value = round(0.15*input$font_size)
        )
      })

      output$interfaceE2 <- shiny::renderUI({
        shiny::req(input$maintabs == "met")
        shiny::sliderInput(
          inputId = "outlier_size",
          label = "Outlier point size (relative)",
          min = 0,
          max = 1,
          value = 0.5
        )
      })

      output$interfaceF <- shiny::renderUI({
        shiny::req(input$maintabs == "met" & input$tabs == "BD")
        shiny::radioButtons(
          inputId = "BD_choice",
          label = "Select instrument",
          choiceNames = c("MAX/FLEX", "SPRINT"),
          choiceValues = c(2.25, 1.8),
          inline = TRUE
        )
      })

      output$interfaceG <- shiny::renderUI({
        shiny::req(input$maintabs == "met")
        shiny::req(input$BD_choice)
        outliers_thresholds <- get(x = "outliers_thresholds", envir = outliers_env)
        ranges <- c(
          "BD" = c(0.1, input$BD_choice, unname(outliers_thresholds[["BD"]])),
          "FoV" = c(50, 100, unname(outliers_thresholds[["FoV"]])),
          "LoD" = c(0, 30, unname(outliers_thresholds[["LoD"]])),
          "PC" = c(0.5, 1, unname(outliers_thresholds[["PC"]]))
        )
        shiny::req(input$tabs)
        if (input$tabs == "BD") {
           shiny::sliderInput(
            inputId = "threshold",
            label = sprintf("Custom QC threshold (default: %s - %s)", ranges["BD1"], ranges["BD2"]),
            min = as.numeric(ranges["BD1"]),
            max = as.numeric(ranges["BD2"]),
            value = c(
              as.numeric(ranges["BD3"]),
              as.numeric(ranges["BD4"])
            )
          )
        } else if (input$tabs == "FoV" | input$tabs == "LoD" | input$tabs == "PC") {
          shiny::sliderInput(
            inputId = "threshold",
            label = sprintf("Custom QC threshold (default: %s)", as.numeric(ranges[paste0(input$tabs, "3")])),
            min = as.numeric(ranges[paste0(input$tabs, "1")]),
            max = as.numeric(ranges[paste0(input$tabs, "2")]),
            value = as.numeric(ranges[paste0(input$tabs, "3")])
          )
        }
      })

      shiny::observeEvent(c(input$tabs, input$threshold), {
        outliers_thresholds <- get(x = "outliers_thresholds", envir = outliers_env)
        outliers_thresholds[[input$tabs]] <- input$threshold
        assign(x = "outliers_thresholds", value = outliers_thresholds, envir = outliers_env)
        saveRDS(object = outliers_thresholds, file = paste0(tempdir(), "/outliers_thresholds.rds"))
      })

      output$interfaceH <- shiny::renderUI({
        shiny::req(input$maintabs %in% c("cg", "norm"))
        shiny::req(input$tabs %in% c("Control Probe Expression", "norm_res"))
        shiny::div(
          shiny::checkboxInput(
            inputId = "with_smooth",
            label = "Smooth line (loess)",
            value = 0
          ),
          align = "center"
        )
      })

      output$outlier_table <- shiny::renderDataTable({
        shiny::req(input$maintabs%in%c("ot"))
        details_out <- details_outlier(
          nacho_df = nacho,
          id_colname = id_colname,
          outliers_thresholds = get(x = "outliers_thresholds", envir = outliers_env)
        )
        all_out <- unique(unlist(details_out))

        data.frame(
          "Accession" = all_out,
          "BD" = ifelse(all_out %in% details_out[["binding_out"]], "FAIL", "PASS"),
          "FOV" = ifelse(all_out %in% details_out[["fov_out"]], "FAIL", "PASS"),
          "PL" = ifelse(all_out %in% details_out[["pc_out"]], "FAIL", "PASS"),
          "LOD" = ifelse(all_out %in% details_out[["lod_out"]], "FAIL", "PASS"),
          "PSF" = ifelse(all_out %in% details_out[["fac_out"]], "FAIL", "PASS"),
          "HSF" = ifelse(all_out %in% details_out[["house_out"]], "FAIL", "PASS")
        )
      })

      output$all <- shiny::renderPlot({
        shiny::req(!input$maintabs%in%c("ot", "about"))
        # Prepare axis text
        labels <- c(
          "MC" = "Average Counts",
          "MedC" = "Median Counts",
          "BD" = "Binding Density",
          "FoV" = "Field of View",
          "PC" = "Positive Control linearity",
          "LoD" = "Limit of Detection"
        )
        units <- c(
          "BD" = '"(Optical features / ", mu, m^2, ")"',
          "FoV" = '"(% Counted)"',
          "PC" = '(R^2)',
          "LoD" = '"(Z)"'
        )

        # Set defaults
        shiny::req(input$maintabs)
        shiny::req(input$font_size)
        main <- ifelse(is.null(input$maintabs), "met", input$maintabs)

        p <- switch(
          EXPR = main,
          "met" = {
            shiny::req(input$tabs)
            shiny::req(input$threshold)
            shiny::req(input$tabs == "FoV" | input$tabs == "LoD" | input$tabs == "PC" | input$tabs == "BD")
            shiny::req(input$attribute, input$meta)
            colour_name <- input$meta

            # Defaults for every main tab
            if (input$tabs == "BD") {
              local_data <- nacho[nacho[[input$tabs]] >= input$threshold[1] & nacho[[input$tabs]] <= input$threshold[2], ]
              outliers_data <- nacho[nacho[[input$tabs]] <= input$threshold[1] | nacho[[input$tabs]] >= input$threshold[2], ]
            } else if (input$tabs == "FoV" | input$tabs == "LoD" | input$tabs == "PC") {
              local_data <- nacho[nacho[[input$tabs]] >= input$threshold[1], ]
              outliers_data <- nacho[nacho[[input$tabs]] <= input$threshold[1], ]
            }
            if (input$tabs %in% c("PC", "LoD")) {
              shiny::req(!all(local_data[[input$tabs]]==0))
            }

            local_data <- dplyr::distinct(.data = local_data[, c(id_colname, input$attribute, input$tabs, colour_name)])
            outliers_data <- dplyr::distinct(.data = outliers_data[, c(id_colname, input$attribute, input$tabs, colour_name)])
            if (!is.character(outliers_data[[colour_name]])) {
              outliers_data[[colour_name]] <- as.character(outliers_data[[colour_name]])
            }
            local_data[[input$attribute]] <- factor(
              x = local_data[[input$attribute]],
              levels = gtools::mixedsort(unique(local_data[[input$attribute]]))
            )
            outliers_data[[input$attribute]] <- factor(
              x = outliers_data[[input$attribute]],
              levels = gtools::mixedsort(unique(outliers_data[[input$attribute]]))
            )

            shiny::req(nrow(local_data)!=0)
            p <- ggplot2::ggplot(
              data = local_data,
              mapping = ggplot2::aes_string(x = input$attribute, y = input$tabs, colour = input$meta)
            ) +
              ggplot2::theme_grey(base_size = input$font_size) +
              ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
              ggbeeswarm::geom_quasirandom(
                width = 0.25,
                size = input$point_size,
                na.rm = TRUE,
                groupOnX = TRUE
              ) +
              ggplot2::theme(
                legend.position = "bottom",
                axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
              ) +
              ggplot2::labs(
                x = input$attribute,
                # y = paste(labels[input$tabs], units[input$tabs], sep = "\n"),
                y = parse(text = paste0('paste("', labels[input$tabs], '", " ", ',  units[input$tabs], ")")),
                colour = input$meta
              )

            if (input$tabs == "BD") {
              p <- p +
                ggplot2::geom_hline(
                  mapping = ggplot2::aes(yintercept = input$threshold[1]),
                  colour = "red",
                  linetype = "longdash",
                  na.rm = TRUE
                ) +
                ggplot2::geom_hline(
                  mapping = ggplot2::aes(yintercept = input$threshold[2]),
                  colour = "red",
                  linetype = "longdash",
                  na.rm = TRUE
                )
            } else {
              p <- p +
                ggplot2::geom_hline(
                  mapping = ggplot2::aes(yintercept = input$threshold[1]),
                  colour = "red",
                  linetype = "longdash",
                  na.rm = TRUE
                )
            }

            # Show outlier
            if (input$outlier) {
              shiny::req(input$outlier)
              p <- p +
                ggbeeswarm::geom_quasirandom(
                  data = outliers_data,
                  mapping = ggplot2::aes_string(x = input$attribute, y = input$tabs),
                  colour = "red",
                  width = 0.25,
                  size = input$outlier_size*input$point_size,
                  na.rm = TRUE,
                  groupOnX = TRUE
                )
            }

            # Show outlier label
            if (input$outlab) {
              shiny::req(input$outlab)
              p <- p +
                ggrepel::geom_label_repel(
                  data = outliers_data,
                  mapping = ggplot2::aes_string(x = input$attribute, y = input$tabs, label = id_colname),
                  colour = "red",
                  inherit.aes = FALSE
                )
            }

            n_colour_levels <- length(unique(local_data[[input$meta]]))
            if (n_colour_levels<=input$max_factors) {
              p <- p + ggplot2::guides(
                colour = ggplot2::guide_legend(ncol = min(n_colour_levels, 10), byrow = TRUE)
              )
            } else {
              p <- p + ggplot2::guides(colour = "none")
            }

            p
          },
          "cg" = {
            shiny::req(input$tabs)
            shiny::req(input$colour_choice)
            shiny::req(input$meta)
            if (input$tabs == "Control Probe Expression") {
              shiny::req(!is.null(input$with_smooth))
              local_data <- nacho[nacho[["CodeClass"]] %in% c("Positive", "Negative"), ]
              local_data <- dplyr::distinct(
                .data = local_data[, c(id_colname, "Count", "CodeClass", "Name")]
              )
              local_data[["Count"]] <- local_data[["Count"]] + 1

              shiny::req(nrow(local_data)!=0)
              p <- ggplot2::ggplot(
                data = local_data,
                mapping = ggplot2::aes_string(
                  x = id_colname,
                  y = "Count",
                  colour = "Name",
                  group = "Name"
                )
              ) +
                ggplot2::theme_grey(base_size = input$font_size) +
                ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                ggplot2::geom_line() +
                ggplot2::facet_wrap(facets = "CodeClass", scales = "free_y") +
                ggplot2::scale_y_log10(limits = c(1, NA)) +
                ggplot2::scale_x_discrete(labels = NULL) +
                ggplot2::labs(x = "Sample Index", y = "Counts + 1", colour = "Control") +
                ggplot2::guides(colour = ggplot2::guide_legend(nrow = 8)) +
                ggplot2::theme(
                  axis.ticks.x = ggplot2::element_blank(),
                  panel.grid.major.x = ggplot2::element_blank(),
                  panel.grid.minor.x = ggplot2::element_blank()
                )

              if (input$with_smooth) {
                p <- p +
                  ggplot2::geom_smooth(
                    mapping = ggplot2::aes(
                      x = as.numeric(as.factor(local_data[[id_colname]])),
                      y = local_data[["Count"]],
                      linetype = rep("Loess", length(local_data[["Count"]]))
                    ),
                    colour = "black",
                    se = TRUE,
                    method = "loess",
                    inherit.aes = FALSE
                  ) +
                  ggplot2::labs(linetype = "Smooth")
              }

              p
            } else {
              shiny::req(input$point_size)
              shiny::req(input$meta)
              shiny::req(input$tabs)
              shiny::req(input$font_size)
              shiny::req(input$max_factors)
              colour_name <- input$meta

              local_data <- nacho[nacho[["CodeClass"]] %in% input$tabs, ]
              local_data <- dplyr::distinct(
                .data = local_data[, c(id_colname, "Name", "Count", colour_name)]
              )
              local_data[["Count"]] <- local_data[["Count"]] + 1
              if (!is.character(local_data[[colour_name]])) {
                local_data[[colour_name]] <- as.character(local_data[[colour_name]])
              }

              shiny::req(nrow(local_data)!=0)
              p <- ggplot2::ggplot(
                data = local_data,
                mapping = ggplot2::aes_string(
                  x = "Name",
                  y = "Count",
                  colour = colour_name
                )
              ) +
                ggplot2::theme_grey(base_size = input$font_size) +
                ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                ggbeeswarm::geom_quasirandom(size = input$point_size, width = 0.25, na.rm = TRUE, groupOnX = TRUE) +
                ggplot2::scale_y_log10(limits = c(1, NA)) +
                ggplot2::labs(
                  colour = colour_name,
                  x = if (input$tabs%in%c("Negative", "Positive")) "Control Name" else "Gene Name",
                  y = "Counts + 1"
                ) +
                ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic"))

              if (is.character(unique(local_data[[colour_name]])) & length(unique(local_data[[colour_name]]))>input$max_factors) {
                p <- p + ggplot2::guides(colour = "none")
              } else {
                p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
              }

              if (length(unique(local_data[["Name"]]))>10) {
                p <- p +
                  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
              }

              p
            }
          },
          "vis" = {
            shiny::req(input$tabs)
            shiny::req(input$colour_choice)
            shiny::req(input$meta)
            colour_name <- input$meta

            p <- switch(
              EXPR = input$tabs,
              "prin" = {
                shiny::req(input$pcA_sel)
                shiny::req(input$pcB_sel)
                # PRINCIPAL COMPONENT PLOT
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, input$pcA_sel, input$pcB_sel, colour_name)]
                )
                if (!is.character(local_data[[colour_name]])) {
                  local_data[[colour_name]] <- as.character(local_data[[colour_name]])
                }

                shiny::req(nrow(local_data)!=0)
                p_point <- ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = input$pcA_sel, y = input$pcB_sel, colour = colour_name)
                ) +
                  ggplot2::theme_grey(base_size = input$font_size) +
                  ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(x = input$pcA_sel, y = input$pcB_sel, colour = colour_name) +
                  ggplot2::stat_ellipse(na.rm = TRUE)

                if (is.character(unique(nacho[[colour_name]])) & length(unique(nacho[[colour_name]]))>input$max_factors) {
                  p_point <- p_point + ggplot2::guides(colour = "none")
                } else {
                  p_point <- p_point + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
                }
                pc_sum[["ProportionofVariance"]] <- pc_sum[["Proportion of Variance"]]
                pc_sum[["PoV"]] <- scales::percent(pc_sum[["Proportion of Variance"]])

                shiny::req(nrow(pc_sum)!=0)
                p_histo <- ggplot2::ggplot(
                  data = pc_sum,
                  mapping = ggplot2::aes_string(x = "PC", y = "ProportionofVariance")
                ) +
                  ggplot2::theme_grey(base_size = input$font_size) +
                  ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                  ggplot2::geom_bar(stat = "identity") +
                  ggplot2::geom_text(
                    mapping = ggplot2::aes_string(label = "PoV"),
                    vjust = -1,
                    show.legend = FALSE
                  ) +
                  ggplot2::scale_y_continuous(
                    labels = scales::percent,
                    expand = ggplot2::expand_scale(mult = c(0, 0.15))
                  ) +
                  ggplot2::labs(x = "Number of Principal Component", y = "Proportion of Variance")

                ggpubr::ggarrange(p_point, p_histo, nrow = 2, ncol = 1)
              },
              "MC-BD" = {
                # Count vs. binding
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, "MC", "BD", colour_name)]
                )

                shiny::req(nrow(local_data)!=0)
                ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = "MC", y = "BD", colour = colour_name)
                ) +
                  ggplot2::theme_grey(base_size = input$font_size) +
                  ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(
                    x = labels["MC"],
                    y = parse(text = paste0('paste("', labels["BD"], '", " ", ',  units["BD"], ")")),
                    colour = colour_name
                  )
              },
              "MC-MedC" = {
                # Count vs. median
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, "MC", "MedC", colour_name)]
                )

                shiny::req(nrow(local_data)!=0)
                ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = "MC", y = "MedC", colour = colour_name)
                ) +
                  ggplot2::theme_grey(base_size = input$font_size) +
                  ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(
                    x = labels["MC"],
                    y = labels["MedC"],
                    colour = colour_name
                  )
              }
            )

            shiny::req(p)
            if (is.character(unique(nacho[[colour_name]])) & length(unique(nacho[[colour_name]]))>input$max_factors) {
              p <- p + ggplot2::guides(colour = "none")
            } else {
              p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
            }

            p
          },
          "norm" = {
            shiny::req(input$tabs%in%c("pfbt", "hf", "norm_res"))
            shiny::req(input$meta)
            colour_name <- input$meta

            p <- switch(
              EXPR = input$tabs,
              "pfbt" = {
                shiny::req(input$point_size, input$font_size, input$max_factors)
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, "Negative_factor", "Positive_factor", colour_name)]
                )
                if (!is.character(local_data[[colour_name]])) {
                  local_data[[colour_name]] <- as.character(local_data[[colour_name]])
                }

                shiny::req(nrow(local_data)!=0)
                p <- ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = "Negative_factor", y = "Positive_factor", colour = colour_name)
                ) +
                  ggplot2::theme_grey(base_size = input$font_size) +
                  ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(x = "Negative Factor", y = "Positive Factor", colour = colour_name) +
                  ggplot2::scale_y_log10()

                if (is.character(unique(local_data[[colour_name]])) & length(unique(local_data[[colour_name]]))>input$max_factors) {
                  p <- p + ggplot2::guides(colour = "none")
                } else {
                  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
                }

                p
              },
              "hf" = {
                shiny::req(housekeeping_norm)
                shiny::req(input$point_size, input$font_size, input$max_factors)
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, "Positive_factor", "House_factor", colour_name)]
                )

                shiny::req(nrow(local_data)!=0)
                p <- ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = "Positive_factor", y = "House_factor", colour = colour_name)
                ) +
                  ggplot2::theme_grey(base_size = input$font_size) +
                  ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(x = "Positive Factor", y = "Housekeeping Factor", colour = colour_name) +
                  ggplot2::scale_x_log10() +
                  ggplot2::scale_y_log10()

                if (is.character(unique(local_data[[colour_name]])) & length(unique(local_data[[colour_name]]))>input$max_factors) {
                  p <- p + ggplot2::guides(colour = "none")
                } else {
                  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
                }

                p
              },
              "norm_res" = {
                shiny::req(!is.null(input$with_smooth))
                shiny::req(input$font_size, input$max_factors)
                if (is.null(housekeeping_genes)) {
                  local_data <- nacho[nacho[["CodeClass"]]%in%"Positive", ]
                } else {
                  local_data <- nacho[nacho[["Name"]] %in% housekeeping_genes, ]
                }

                local_data[["Count_Norm"]] <- normalise_counts(
                  data = local_data,
                  housekeeping_norm = housekeeping_norm
                )
                local_data <- dplyr::distinct(
                  .data = local_data[, c(id_colname, "Count", "Count_Norm", "Name")]
                )

                local_data <- tidyr::gather(data = local_data, key = "Status", value = "Count", c("Count", "Count_Norm"))
                local_data[["Status"]] <- factor(
                  x = c("Count" = "Raw", "Count_Norm" = "Normalised")[local_data[["Status"]]],
                  levels = c("Count" = "Raw", "Count_Norm" = "Normalised")
                )
                local_data[["Count"]] <- local_data[["Count"]] + 1

                shiny::req(nrow(local_data)!=0)
                p <- ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(
                    x = id_colname,
                    y = "Count",
                    colour = "Name",
                    group = "Name"
                  )
                ) +
                  ggplot2::theme_grey(base_size = input$font_size) +
                  ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
                  ggplot2::geom_line(na.rm = TRUE) +
                  ggplot2::facet_grid(~Status) +
                  ggplot2::scale_x_discrete(label = NULL) +
                  ggplot2::scale_y_log10(limits = c(1, NA)) +
                  ggplot2::labs(
                    x = "Sample Index",
                    y = "Counts + 1",
                    colour = if (is.null(housekeeping_genes)) "Positive Control" else "Housekeeping Genes"
                  ) +
                  ggplot2::theme(
                    axis.ticks.x = ggplot2::element_blank(),
                    panel.grid.major.x = ggplot2::element_blank(),
                    panel.grid.minor.x = ggplot2::element_blank()
                  )

                if (input$with_smooth) {
                  p <- p +
                    ggplot2::geom_smooth(
                      mapping = ggplot2::aes(
                        x = as.numeric(as.factor(local_data[[id_colname]])),
                        y = local_data[["Count"]],
                        linetype = rep("Loess", length(local_data[["Count"]]))
                      ),
                      colour = "black",
                      se = TRUE,
                      method = "loess",
                      inherit.aes = FALSE,
                      na.rm = TRUE
                    ) +
                    ggplot2::labs(linetype = "Smooth")
                }

                if (is.character(unique(local_data[["Name"]])) & length(unique(local_data[["Name"]]))>input$max_factors) {
                  p <- p + ggplot2::guides(colour = "none")
                } else {
                  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
                }

                p
              }
            )
          }
        )

        print(p)
      })
    }
  )

  shiny::runApp(app)
}



#' @export
#' @rdname visualise
#' @usage NULL
visualize <- visualise
