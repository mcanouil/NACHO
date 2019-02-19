server <- function(input, output) {
  shiny::observeEvent(input$do, {
    file_ext <- function (x) {
        pos <- regexpr("\\.([[:alnum:]]+)$", x)
        ifelse(pos > -1L, substring(x, pos + 1L), "")
    }
    clean_path <- normalizePath(input$save_path)
    if (!dir.exists(clean_path)) {
      dir.create(path = clean_path)
    }
    ggplot2::ggsave(
      filename = paste0(
        clean_path, "/",
        input$name, if (file_ext(input$name)=="") ".pdf"
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
    details_out <- NACHO:::details_outlier(nacho_df = nacho, id_colname = id_colname)
    all_out <- unique(unlist(details_out))

    outlier_table <- data.frame(
      "Accession" = all_out,
      "BD" = ifelse(all_out %in% details_out[["binding_out"]], "FAIL", "PASS"),
      "FOV" = ifelse(all_out %in% details_out[["fov_out"]], "FAIL", "PASS"),
      "PL" = ifelse(all_out %in% details_out[["pc_out"]], "FAIL", "PASS"),
      "LOD" = ifelse(all_out %in% details_out[["lod_out"]], "FAIL", "PASS"),
      "PSF" = ifelse(all_out %in% details_out[["fac_out"]], "FAIL", "PASS"),
      "HSF" = ifelse(all_out %in% details_out[["house_out"]], "FAIL", "PASS")
    )
    return(outlier_table)
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
      "BD" = "(Optical features / μm²)",
      "FoV" = "(%Counted)",
      "PC" = "(R²)",
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
          req(!all(local_data[[input$tabs]]==0))
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

        return(p)
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
            mapping = ggplot2::aes(x = get(id_colname), y = Count+1, colour = Name, group = Name)
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
            mapping = ggplot2::aes(x = Name, y = Count+1, colour = get(colour_name))
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
          return(p)
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

            ggpubr::ggarrange(p_point, p_histo, nrow = 2, ncol = 1)
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
              mapping = ggplot2::aes(x = MC, y = MedC, colour = get(colour_name))
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

        req(p)
        if (is.character(unique(nacho[[colour_name]])) & length(unique(nacho[[colour_name]]))>40) {
          p <- p + ggplot2::guides(colour = "none")
        } else {
          p <- p + ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2))
        }
        return(p)
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
          mapping = ggplot2::aes(x = Negative_factor, y = Positive_factor, colour = get(colour_name))
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
            mapping = ggplot2::aes(x = Positive_factor, y = House_factor, colour = get(colour_name))
          ) +
            ggplot2::geom_point(size = input$point_size, na.rm = TRUE) +
            ggplot2::labs(x = "Positive Factor", y = "Houskeeping factor", colour = colour_name) +
            ggplot2::scale_x_log10() +
            ggplot2::scale_y_log10()
        } else if (input$tabs == "norm_res") {
          local_data <- dplyr::bind_rows(
            nacho[nacho[["Name"]] %in% housekeeping_genes, ],
            tibble::tibble(
              "CodeClass" = "Average",
              "Name" = "Mean",
              "Accession" = "nacho",
              "Count" = nacho[["MC"]],
              !!id_colname := nacho[[id_colname]]
            )
          )
          local_data[["Count_Norm"]] <- NACHO:::normalise_counts(
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
            mapping = ggplot2::aes(x = get(id_colname), y = Count + 1, colour = Name, group = Name)
          ) +
            ggplot2::geom_line() +
            ggplot2::facet_grid(~Status) +
            ggplot2::scale_x_discrete(label = NULL) +
            ggplot2::scale_y_log10(limits = c(1, NA)) +
            ggplot2::labs(x = "Sample index", y = "Counts + 1") +
            ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), panel.grid.minor.x = ggplot2::element_blank())
        }
        return(p)
      }
    )

    print(p)
  })
}
