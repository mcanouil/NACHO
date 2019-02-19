#' summarise
#'
#' @param data_directory [character]
#' @param ssheet_csv [character]
#' @param id_colname [character]
#' @param housekeeping_genes [vector(character)]
#' @param housekeeping_predict [logical]
#' @param housekeeping_norm [logical]
#' @param normalisation_method [character]
#' @param n_comp [numeric]
#'
#' @return [list]
#' @export
#'
#' @examples NULL
summarise <- function(
  data_directory = NULL,
  ssheet_csv = NULL,
  id_colname = NULL,
  housekeeping_genes = NULL,
  housekeeping_predict = FALSE,
  housekeeping_norm = TRUE,
  normalisation_method = "GEO",
  n_comp = 10
) {
  data_directory <- normalizePath(data_directory)
  nacho_df <- utils::read.csv(file = ssheet_csv, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  nacho_df <- tibble::as_tibble(nacho_df)
  nacho_df[["file_path"]] <- paste(data_directory, nacho_df[[id_colname]], sep = "/")
  nacho_df[["file_exists"]] <- sapply(X = nacho_df[["file_path"]], FUN = file.exists)
  nacho_df[["rcc_content"]] <- lapply(X = nacho_df[["file_path"]], FUN = read_rcc)

  column_to_unnest <- c("rcc_content", "Code_Summary")
  nacho_df <- tidyr::unnest(data = nacho_df, rcc_content = get(column_to_unnest[1]), .drop = FALSE)
  nacho_df <- tidyr::unnest(data = nacho_df, Code_Summary = get(column_to_unnest[2]), .drop = FALSE)
  nacho_df[["CodeClass"]] <- gsub("Endogenous.*", "Endogenous", nacho_df[["CodeClass"]])

  if ("plexset_id" %in% colnames(nacho_df)) {
    nacho_df <- tidyr::unite(data = nacho_df, col = !!id_colname, id_colname, "plexset_id")
  }

  qc_rcc(
    data_directory = data_directory,
    nacho_df = nacho_df,
    id_colname = id_colname,
    housekeeping_genes = housekeeping_genes,
    housekeeping_predict = housekeeping_predict,
    housekeeping_norm = housekeeping_norm,
    normalisation_method = normalisation_method,
    n_comp = n_comp
  )
}


#' @export
#' @rdname summarise
#' @usage NULL
summarize <- summarise


#' normalise
#'
#' @param nacho_object [list]
#' @param housekeeping_genes [vector(character)]
#' @param housekeeping_norm [logical]
#' @param normalisation_method [character]
#' @param remove_outliers [logical]
#'
#' @return [list]
#' @export
#'
#' @examples NULL
normalise <- function(
  nacho_object,
  housekeeping_genes = nacho_object[["housekeeping_genes"]],
  housekeeping_norm = nacho_object[["housekeeping_norm"]],
  normalisation_method = nacho_object[["normalisation_method"]],
  remove_outliers = TRUE
) {
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
    "nacho"
  )
  if (!all(mandatory_fields%in%names(nacho_object))) {
    stop('[NACHO::normalise] No valid data provided. \n Use "summarise()" or "summarize()" to generate data!')
  }

  id_colname <- nacho_object[["access"]]

  if (!isTRUE(all.equal(sort(nacho_object[["housekeeping_genes"]]), sort(housekeeping_genes)))) {
    warning(
      paste0(
        '[NACHO::normalise] "housekeeping_genes" is different from the parameter used to import RCC files!\n',
        '    "summarise()" parameter:\n',
        '        housekeeping_genes=', deparse(nacho_object[["housekeeping_genes"]]), '\n',
        '    "normalise()" parameter:\n',
        '        housekeeping_genes=', deparse(housekeeping_genes), '\n'
      )
    )
  }

  if (nacho_object[["housekeeping_norm"]]!=housekeeping_norm) {
    warning(
      paste0(
        '[NACHO::normalise] "housekeeping_norm" is different from the parameter used to import RCC files!\n',
        '    "summarise()" parameter:\n',
        '        housekeeping_norm=', deparse(nacho_object[["housekeeping_norm"]]), '\n',
        '    "normalise()" parameter:\n',
        '        housekeeping_norm=', deparse(housekeeping_norm), '\n'
      )
    )
  }

  if (nacho_object[["normalisation_method"]]!=normalisation_method) {
    warning(
      paste0(
        '[NACHO::normalise] "normalisation_method" is different from the parameter used to import RCC files!\n',
        '    "summarise()" parameter:\n',
        '        normalisation_method=', deparse(nacho_object[["normalisation_method"]]), '\n',
        '    "normalise()" parameter:\n',
        '        normalisation_method=', deparse(normalisation_method), '\n'
      )
    )
  }

  if (nacho_object[["remove_outliers"]]) {
    message("Outliers have already been removed!")
  }

  if (remove_outliers & !nacho_object[["remove_outliers"]]) {
    nacho_df <- exclude_outliers(object = nacho_object)
    outliers <- setdiff(
      unique(nacho_object[["nacho"]][[nacho_object[["access"]]]]),
      unique(nacho_df[[nacho_object[["access"]]]])
    )
    if (length(outliers)!=0) {
      nacho_object <- qc_rcc(
        data_directory = nacho_object[["data_directory"]],
        nacho_df = nacho_df,
        id_colname = id_colname,
        housekeeping_genes = housekeeping_genes,
        housekeeping_predict = FALSE,
        housekeeping_norm = housekeeping_norm,
        normalisation_method = normalisation_method,
        n_comp = nacho_object[["n_comp"]]
      )
    }
    nacho_object[["remove_outliers"]] <- remove_outliers
  }

  nacho_object[["nacho"]][["Count_Norm"]] <- normalise_counts(
    data = nacho_object[["nacho"]],
    housekeeping_norm = housekeeping_norm
  )

  raw_counts <- format_counts(
    data = nacho_object[["nacho"]],
    id_colname = id_colname,
    count_column = "Count"
  )
  nacho_object[["raw_counts"]] <- raw_counts

  norm_counts <- format_counts(
    data = nacho_object[["nacho"]],
    id_colname = id_colname,
    count_column = "Count_Norm"
  )
  nacho_object[["normalised_counts"]] <- norm_counts

  nacho_object
}


#' @export
#' @rdname normalise
#' @usage NULL
normalize <- normalise


#' visualise
#'
#' @param nacho_object [list]
#'
#' @return [NULL]
#' @export
#'
#' @examples NULL
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
    "nacho" # ,
    # "raw_counts",
    # "normalised_counts"
  )
  if (!interactive()) {
    stop('[NACHO::visualise] Must be run in interactive R session!')
  }
  if (!all(mandatory_fields%in%names(nacho_object))) {
    stop('[NACHO::visualise] No valid data provided. \n Use "summarise()" or "summarize()" to generate data!')
  }

  font_size <- 14
  ggplot2::theme_set(ggplot2::theme_grey(base_size = font_size))

  id_colname <- nacho_object[["access"]]
  housekeeping_genes <- nacho_object[["housekeeping_genes"]]
  housekeeping_norm <- nacho_object[["housekeeping_norm"]]
  pc_sum <- nacho_object[["pc_sum"]]
  nacho <- nacho_object[["nacho"]]
  save_path_default <- nacho_object[["data_directory"]]

  shiny::addResourcePath("www", system.file("logo", package = "NACHO"))

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::img(src = "www/Nacho_logo.png", height = 150)
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
          shiny::uiOutput(outputId = "interfaceE1"),
          shiny::uiOutput(outputId = "interfaceE2"),
          shiny::uiOutput(outputId = "interfaceF"),
          shiny::uiOutput(outputId = "interfaceG"),
          shiny::tabsetPanel(
            id = "down",
            shiny::tabPanel(
              title = "Download",
              shiny::numericInput(inputId = "w", label = "Width:", width = 70, value = 8),
              shiny::numericInput(inputId = "h", label = "Height:", width = 70, value = 6),
              shiny::textInput(inputId = "save_path", label = "Output directory:", value = save_path_default),
              shiny::textInput(inputId = "name", label = "Plot name:"),
              shiny::actionButton(inputId = "do", label = "Download")
            )
          )
        ),
        shiny::column(
          width = 9,
          shiny::plotOutput(outputId = "all", width = "100%", height = "400px")
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
          dpi = 300
        )
        shiny::showNotification(paste0('Plot successfully saved to: "', clean_path, '"'))
      })

      # Render subtabs based on maintab
      output$subtab <- shiny::renderUI({
        shiny::req(input$maintabs!="ot")
        switch(
          EXPR = input$maintabs,
          "met" = {
            shiny::tabsetPanel(
              id = "tabs",
              shiny::tabPanel("Binding Density", value = "BD"),
              shiny::tabPanel("Imaging", value = "FoV"),
              shiny::tabPanel("Positive Control Linearity", value = "PC"),
              shiny::tabPanel("Limit of Detection", value = "LoD")
            )
          },
          "cg" = {
            tabslist <- unique(nacho[["CodeClass"]][-grep("Endogenous", nacho[["CodeClass"]])])
            tabslist <- c(tabslist, "Control probe expression")
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
            shiny::tabsetPanel(
              id = "tabs",
              shiny::tabPanel("Positive factor vs Background threshold", value = "pfbt"),
              shiny::tabPanel("Housekeeping factor", value = "hf"),
              shiny::tabPanel("Normalisation result", value = "norm_res")
            )
          },
          "vis" = {
            shiny::tabsetPanel(
              id = "tabs",
              shiny::tabPanel("Average Count vs Binding Density", value = "MC-BD"),
              shiny::tabPanel("Average Count vs Median Count", value = "MC-MedC"),
              shiny::tabPanel("Principal Component", value = "prin")
            )
          },
          "about" = {shiny::p(shiny::br(), "Work in progress!")}
        )
      })

      # Render interface options based on maintab
      output$interfaceA <- shiny::renderUI({
        shiny::req(input$maintabs)
        if (input$maintabs == "met") {
          shiny::selectInput(
            inputId = "Attribute",
            label = "Select x-axis",
            choices = c(
              "Date",
              "ID",
              "CartridgeID",
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
              inputId = "Attribute",
              label = "Coloured by:",
              choices = c(
                "Date",
                "ID",
                "CartridgeID",
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
          shiny::checkboxInput(
            inputId = "outlier",
            label = "View outliers",
            value = TRUE
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
          shiny::checkboxInput(
            inputId = "outlab",
            label = "View outlier labels"
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
        shiny::req(input$maintabs == "met")
        shiny::sliderInput(
          inputId = "point_size",
          label = "Point size",
          min = 1,
          max = 5,
          value = 0.25*font_size
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
          choiceValues = c(2.25, 1.8)
        )
      })

      output$interfaceG <- shiny::renderUI({
        shiny::req(input$maintabs == "met")
        shiny::req(input$BD_choice)
        ranges <- c(
          "BD" = c(0.1, 4, 0.1, input$BD_choice),
          "FoV" = c(50, 100, 75),
          "LoD" = c(0, 30, 2),
          "PC" = c(0.5, 1, 0.95)
        )
        shiny::req(input$tabs)
        if (input$tabs == "BD") {
          shiny::sliderInput(
            inputId = "threshold",
            label = "Custom QC threshold",
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
            label = "Custom QC threshold",
            min = as.numeric(ranges[paste0(input$tabs, "1")]),
            max = as.numeric(ranges[paste0(input$tabs, "2")]),
            value = as.numeric(ranges[paste0(input$tabs, "3")])
          )
        }
      })

      output$outlier_table <- shiny::renderDataTable({
        details_out <- details_outlier(nacho_df = nacho, id_colname = id_colname)
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
          "BD" = "(Optical features / um2)",
          "FoV" = "(%Counted)",
          "PC" = "(R2)",
          "LoD" = "(Z)"
        )

        # Set defaults
        shiny::req(input$maintabs)
        main <- ifelse(is.null(input$maintabs), "met", input$maintabs)

        p <- switch(
          EXPR = main,
          "met" = {
            shiny::req(input$tabs)
            shiny::req(input$threshold)
            shiny::req(input$tabs == "FoV" | input$tabs == "LoD" | input$tabs == "PC" | input$tabs == "BD")

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

            local_data <- dplyr::distinct(.data = local_data[, c(id_colname, input$Attribute, input$tabs, input$meta)])
            outliers_data <- dplyr::distinct(.data = outliers_data[, c(id_colname, input$Attribute, input$tabs, input$meta)])

            p <- ggplot2::ggplot(
              data = local_data,
              mapping = ggplot2::aes_string(x = input$Attribute, y = input$tabs, colour = input$meta)
            ) +
              ggbeeswarm::geom_quasirandom(width = 0.2, size = input$point_size, na.rm = TRUE) +
              ggplot2::theme(
                legend.position = "bottom",
                axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
              ) +
              ggplot2::labs(
                x = input$Attribute,
                y = paste(labels[input$tabs], units[input$tabs], sep = "\n"),
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
                ggbeeswarm::geom_beeswarm(
                  data = outliers_data,
                  mapping = ggplot2::aes_string(x = input$Attribute, y = input$tabs),
                  colour = "red",
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
                  mapping = ggplot2::aes_string(x = input$Attribute, y = input$tabs, label = id_colname),
                  colour = "red",
                  inherit.aes = FALSE
                )
            }

            n_colour_levels <- length(unique(local_data[[input$meta]]))
            if (n_colour_levels<=40) {
              p <- p + ggplot2::guides(
                colour = ggplot2::guide_legend(ncol = min(n_colour_levels, 10), byrow = TRUE)
              )
            } else {
              p <- p + ggplot2::guides(colour = "none")
            }

            p
          },
          "cg" = {
            shiny::req(input$colour_choice)
            shiny::req(input$meta)
            shiny::req(input$Attribute)
            shiny::req(input$tabs)
            if (input$tabs == "Control probe expression") {
              local_data <- dplyr::distinct(
                .data = nacho[nacho[["CodeClass"]] %in% c("Positive", "Negative"), c(id_colname, "Count", "CodeClass", "Name")]
              )
              p <- ggplot2::ggplot(
                data = local_data,
                mapping = ggplot2::aes(
                  x = get(id_colname),
                  y = Count+1,
                  colour = Name,
                  group = Name
                )
              ) +
                ggplot2::geom_line() +
                ggplot2::facet_wrap(facets = "CodeClass", scales = "free_y") +
                ggplot2::scale_y_log10(limits = c(1, NA)) +
                ggplot2::scale_x_discrete(labels = NULL) +
                ggplot2::labs(x = "Sample index", y = "Counts + 1", colour = "Control") +
                ggplot2::guides(colour = ggplot2::guide_legend(nrow = 8)) +
                ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())
            } else {
              shiny::req(input$point_size)
              if (input$colour_choice) {
                colour_name <- input$meta
              } else {
                colour_name <- input$Attribute
              }
              local_data <- dplyr::distinct(
                .data = nacho[nacho[["CodeClass"]] %in% input$tabs, c(id_colname, "Name", "Count", colour_name)]
              )
              p <- ggplot2::ggplot(
                data = local_data,
                mapping = ggplot2::aes(
                  x = Name,
                  y = Count+1,
                  colour = get(colour_name)
                )
              ) +
                ggbeeswarm::geom_quasirandom(size = input$point_size, width = 0.5, na.rm = TRUE) +
                ggplot2::scale_y_log10(limits = c(1, NA)) +
                ggplot2::labs(colour = colour_name, x = "Gene Name", y = "Counts + 1") +
                ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic"))

              if (is.character(unique(local_data[[colour_name]])) & length(unique(local_data[[colour_name]]))>40) {
                p <- p + ggplot2::guides(colour = "none")
              } else {
                p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
              }

              p
            }
          },
          "vis" = {
            shiny::req(input$tabs)
            shiny::req(input$colour_choice)
            shiny::req(input$Attribute)
            shiny::req(input$meta)
            if (input$colour_choice) {
              colour_name <- input$meta
            } else {
              colour_name <- input$Attribute
            }
            p <- switch(
              EXPR = input$tabs,
              "prin" = {
                shiny::req(input$pcA_sel)
                shiny::req(input$pcB_sel)
                # PRINCIPAL COMPONENT PLOT
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, input$pcA_sel, input$pcB_sel, colour_name)]
                )

                p_point <- ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = input$pcA_sel, y = input$pcB_sel, colour = colour_name)
                ) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(
                    x = as.character(input$pcA_sel),
                    y = as.character(input$pcB_sel),
                    colour = colour_name
                  ) +
                  ggplot2::stat_ellipse()

                p_histo <- ggplot2::ggplot(
                  data = pc_sum,
                  mapping = ggplot2::aes(x = PC, y = `Proportion of Variance`, group = 1)
                ) +
                  ggplot2::geom_bar(stat = "identity") +
                  ggplot2::geom_text(
                    mapping = ggplot2::aes(label = scales::percent(`Proportion of Variance`)),
                    vjust = -1
                  ) +
                  ggplot2::scale_y_continuous(
                    labels = scales::percent,
                    expand = ggplot2::expand_scale(mult = c(0, 0.15))
                  ) +
                  ggplot2::labs(x = "Number of Principal Component", y = "Proportion of Variance")

                ggpubr::ggarrange(p_point, p_histo, nrow = 2, ncol = 1, align = "h")
              },
              "MC-BD" = {
                # Count vs binding
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, "MC", "BD", colour_name)]
                )
                ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = "MC", y = "BD", colour = colour_name)
                ) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(
                    x = labels["MC"],
                    y = paste(labels["BD"], units["BD"], sep = "\n"),
                    colour = colour_name
                  ) +
                  ggplot2::theme(
                    legend.position = "bottom",
                    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
                  )
              },
              "MC-MedC" = {
                # Count vs median
                local_data <- dplyr::distinct(
                  .data = nacho[, c(id_colname, "MC", "MedC", colour_name)]
                )
                ggplot2::ggplot(
                  data = local_data,
                  mapping = ggplot2::aes_string(x = "MC", y = "MedC", colour = colour_name)
                ) +
                  ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                  ggplot2::labs(
                    x = labels["MC"],
                    y = labels["MedC"],
                    colour = colour_name
                  ) +
                  ggplot2::theme(
                    legend.position = "bottom",
                    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
                  )
              }
            )

            shiny::req(p)
            if (is.character(unique(nacho[[colour_name]])) & length(unique(nacho[[colour_name]]))>40) {
              p <- p + ggplot2::guides(colour = "none")
            } else {
              p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
            }

            p
          },
          "norm" = {
            shiny::req(input$colour_choice)
            shiny::req(input$meta)
            shiny::req(input$Attribute)
            shiny::req(input$tabs)
            if (input$colour_choice) {
              colour_name <- input$meta
            } else {
              colour_name <- input$Attribute
            }
            local_data <- dplyr::distinct(
              .data = nacho[, c(id_colname, "Negative_factor", "Positive_factor", colour_name)]
            )
            p <- ggplot2::ggplot(
              data = local_data,
              mapping = ggplot2::aes_string(x = "Negative_factor", y = "Positive_factor", colour = colour_name)
            ) +
              ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
              ggplot2::labs(x = "Negative Factor", y = "Positive Factor", colour = colour_name) +
              ggplot2::scale_y_log10()

            if (input$tabs == "hf") {
              local_data <- dplyr::distinct(
                .data = nacho[, c(id_colname, "Positive_factor", "House_factor", colour_name)]
              )
              p <- ggplot2::ggplot(
                data = local_data,
                mapping = ggplot2::aes_string(x = "Positive_factor", y = "House_factor", colour = colour_name)
              ) +
                ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
                ggplot2::labs(x = "Positive Factor", y = "Houskeeping factor", colour = colour_name) +
                ggplot2::scale_x_log10() +
                ggplot2::scale_y_log10()
            } else if (input$tabs == "norm_res") {
              out <- tibble::tibble(
                "CodeClass" = "Average",
                "Name" = "Mean",
                "Accession" = "nacho",
                "Count" = nacho[["MC"]]
              )
              out[[id_colname]] <- nacho[[id_colname]]
              local_data <- dplyr::bind_rows(nacho[nacho[["Name"]] %in% housekeeping_genes, ], out)
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
              p <- ggplot2::ggplot(
                data = local_data,
                mapping = ggplot2::aes(
                  x = get(id_colname),
                  y = Count + 1,
                  colour = Name,
                  group = Name
                )
              ) +
                ggplot2::geom_line() +
                ggplot2::facet_grid(~Status) +
                ggplot2::scale_x_discrete(label = NULL) +
                ggplot2::scale_y_log10(limits = c(1, NA)) +
                ggplot2::labs(x = "Sample index", y = "Counts + 1") +
                ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())
            }

            p
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
