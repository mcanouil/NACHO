#' render_nacho
#'
#' @inheritParams normalise
#' @param colour_name [character] Character string of the column in \code{ssheet_csv}
#'   or more generally in \code{nacho_object$nacho}.
#' @param legend [logical] Boolean to indicate whether the plot legends should
#'   be plotted (\code{TRUE}) or not (\code{FALSE}). Default is \code{FALSE}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(NACHO)
#' 
#' data(GSE74821)
#' 
#' render_nacho(nacho_object = GSE74821)
#' }
render_nacho <- function(nacho_object, colour_name = "CartridgeID", legend = FALSE) {
  if (is.numeric(nacho_object$nacho[[colour_name]])) {
    nacho_object$nacho[[colour_name]] <- as.character(nacho_object$nacho[[colour_name]])
  }
  
  cat("# NanoString Quality-Control (NACHO) {.tabset}\n\n")
  cat(paste0("<center>![](", system.file("help", "figures", "nacho_hex.png", package = "NACHO"), "){width=150px}</center>"))
  
  cat("\n\n")
  cat("## RCC Summary {-}\n\n")
  cat('  - Samples:', length(unique(nacho_object$nacho[[nacho_object$access]])), "\n")
  genes <- table(nacho_object$nacho[["CodeClass"]]) / 
    length(unique(nacho_object$nacho[[nacho_object$access]]))
  cat(paste0("  - ", names(genes), ": ", genes, "\n"))
  
  
  cat("\n\n")
  cat("## Settings {-}\n\n")
  cat('  - Predict housekeeping genes:', nacho_object$housekeeping_predict, "\n")
  cat('  - Normalise using housekeeping genes:', nacho_object$housekeeping_norm, "\n")
  cat(
    '  - Housekeeping genes available:', 
    paste(nacho_object$housekeeping_genes[-length(nacho_object$housekeeping_genes)], collapse = ", "),
    "and", 
    nacho_object$housekeeping_genes[length(nacho_object$housekeeping_genes)], "\n"
  )
  cat('  - Normalise using:', nacho_object$normalisation_method, "\n")
  cat('  - Principal components to compute:', nacho_object$n_comp, "\n")
  cat('  - Remove outliers:', nacho_object$remove_outliers, "\n")
  cat(
    "\n",
    '    + ', 'Binding Density (BD) <', 
      round(nacho_object$outliers_thresholds[["BD"]][1], 3), '\n',
    '    + ', 'Binding Density (BD) >', 
      round(nacho_object$outliers_thresholds[["BD"]][2], 3), '\n',
    '    + ', 'Imaging (FoV) <', 
      round(nacho_object$outliers_thresholds[["FoV"]], 3), '\n',
    '    + ', 'Positive Control Linearity (PC) <', 
      round(nacho_object$outliers_thresholds[["PC"]], 3), '\n',
    '    + ', 'Limit of Detection (LoD) <', 
      round(nacho_object$outliers_thresholds[["LoD"]], 3), '\n',
    '    + ', 'Positive normalisation factor (Positive_factor) <', 
      round(nacho_object$outliers_thresholds[["Positive_factor"]][1], 3), '\n',
    '    + ', 'Positive normalisation factor (Positive_factor) >', 
      round(nacho_object$outliers_thresholds[["Positive_factor"]][2], 3), '\n',
    '    + ', 'Housekeeping normalisation factor (house_factor) <', 
      round(nacho_object$outliers_thresholds[["House_factor"]][1], 3), '\n',
    '    + ', 'Housekeeping normalisation factor (house_factor) >', 
      round(nacho_object$outliers_thresholds[["House_factor"]][2], 3), '\n'
  )
  
  
  cat("\n\n")
  cat("## QC Metrics {- .tabset}\n\n")
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

  
  for (imetric in c("BD", "FoV", "PC", "LoD")) {
    cat("\n\n")
    cat("###", labels[imetric], "{-}\n\n")
    p <- ggplot2::ggplot(
      data = nacho_object$nacho %>% 
        dplyr::select(
          CartridgeID, 
          !!colour_name, 
          !!nacho_object$access, 
          !!imetric
        ) %>% 
        dplyr::distinct(),
      mapping = ggplot2::aes_string(
        x = "CartridgeID", 
        y = imetric, 
        colour = colour_name
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggbeeswarm::geom_quasirandom(width = 0.25, na.rm = TRUE, groupOnX = TRUE) +
      {if (!legend) ggplot2::guides(colour = "none")} +
      ggplot2::labs(
        x = "CartridgeID",
        y = parse(text = paste0('paste("', labels[imetric], '", " ", ',  units[imetric], ")"))
      ) +
      ggplot2::geom_hline(
        data = dplyr::tibble(value = nacho_object$outliers_thresholds[[imetric]]),
        mapping = ggplot2::aes_string(yintercept = "value"),
        colour = "firebrick2",
        linetype = "longdash"
      )
    
    print(p)
    cat("\n")
  }
  
  
  cat("\n\n")
  cat("## Control Genes {- .tabset}\n\n")
  for (icodeclass in c("Positive", "Negative", "Housekeeping")) {
    cat("\n\n")
    cat("###", icodeclass, "{-}\n\n")
    p <- ggplot2::ggplot(
      data = nacho_object$nacho %>% 
        dplyr::filter(CodeClass %in% icodeclass) %>% 
        dplyr::select(
          CartridgeID, 
          !!colour_name, 
          !!nacho_object$access, 
          !!imetric, 
          Name, 
          Count
        ) %>% 
        dplyr::distinct(),
      mapping = ggplot2::aes_string(
        x = "Name",
        y = "Count",
        colour = colour_name
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggbeeswarm::geom_quasirandom(width = 0.25, na.rm = TRUE, groupOnX = TRUE) +
      ggplot2::scale_y_log10(limits = c(1, NA)) +
      ggplot2::labs(
        x = if (icodeclass%in%c("Negative", "Positive")) "Control Name" else "Gene Name",
        y = "Counts + 1"
      ) +
      {if (!legend) guides(colour = "none")} +
      ggplot2::theme(axis.text.x = ggplot2::element_text(face = "italic"))
   
    number_ticks_x <- nacho_object$nacho %>% 
      dplyr::filter(CodeClass %in% icodeclass) %>% 
      .$Name %>% 
      unique() %>% 
      length()
    
    if (number_ticks_x > 10 | legend) {
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
    
    print(p)
    cat("\n")
  }
  

  cat("\n\n")
  cat("### Control Probe Expression {-}\n\n")
  p <- ggplot2::ggplot(
    data = nacho_object$nacho %>% 
      dplyr::filter(CodeClass%in%c("Positive", "Negative")) %>% 
      dplyr::select(
        CartridgeID, 
        !!colour_name, 
        !!nacho_object$access, 
        CodeClass, 
        Name, 
        Count
      ) %>% 
      dplyr::distinct(),
    mapping = ggplot2::aes_string(
      x = nacho_object$access,
      y = "Count",
      colour = "Name",
      group = "Name"
    )
  ) +
    ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(facets = "CodeClass", scales = "free_y", ncol = 2) +
    ggplot2::scale_y_log10(limits = c(1, NA)) +
    ggplot2::scale_x_discrete(labels = NULL) +
    ggplot2::labs(x = "Sample Index", y = "Counts + 1", colour = "Control") +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank()
    ) +
    {if (!legend) ggplot2::guides(colour = "none")}
  print(p)
  cat("\n")
  
  
  cat("\n\n")
  cat("## QC Visuals {- .tabset}\n\n")
  
  cat("\n\n")
  cat("### Average Count vs. Binding Density {-}\n\n")
  p <- nacho_object$nacho %>% 
    dplyr::select(
      CartridgeID, 
      !!colour_name, 
      !!nacho_object$access, 
      MC, 
      BD
    ) %>% 
    dplyr::distinct() %>% 
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = "MC", 
        y = "BD", 
        colour = colour_name
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::labs(
        x = labels["MC"],
        y = parse(text = paste0('paste("', labels["BD"], '", " ", ',  units["BD"], ")"))
      )
  print(p)
  cat("\n")
  
  
  cat("\n")
  cat("\n\n")
  cat("### Average Count vs. Median Count {-}\n\n")
  p <- nacho_object$nacho %>% 
    dplyr::select(
      CartridgeID, 
      !!colour_name, 
      !!nacho_object$access,
      MC, 
      MedC
    ) %>% 
    dplyr::distinct() %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = "MC", 
        y = "MedC", 
        colour = colour_name
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point( na.rm = TRUE) +
      ggplot2::labs(
        x = labels["MC"],
        y = labels["MedC"]
      )
  print(p)
  cat("\n")
  
  
  cat("\n")
  cat("\n\n")
  cat("### Principal Component {- .tabset}\n\n")
  
  cat("\n\n")
  cat("#### PC1 vs. PC2 {-}\n\n")
  p <- nacho_object$nacho %>% 
    dplyr::select(
      CartridgeID, 
      !!colour_name, 
      !!nacho_object$access, 
      paste0("PC", 1:2)
    ) %>% 
    dplyr::distinct() %>% 
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = "PC1", 
        y = "PC2", 
        colour = colour_name
      )
    ) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::stat_ellipse(na.rm = TRUE) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      {if (!legend) ggplot2::guides(colour = "none")}
  print(p)
  cat("\n")
  
  cat("\n\n")
  cat("#### Factorial planes {-}\n\n")
  p <- dplyr::full_join(
    x = nacho_object$nacho %>% 
      dplyr::select(
        CartridgeID, 
        !!colour_name, 
        !!nacho_object$access, 
        paste0("PC", 1:min(nacho_object$n_comp, 5))
      ) %>% 
      dplyr::distinct() %>% 
      tidyr::gather(key = "X.PC", value = "X", paste0("PC", 1:min(nacho_object$n_comp, 5))),
    y = nacho_object$nacho %>% 
      dplyr::select(
        CartridgeID, 
        !!colour_name, 
        !!nacho_object$access, 
        paste0("PC", 1:min(nacho_object$n_comp, 5))
      ) %>% 
      dplyr::distinct() %>% 
      tidyr::gather(key = "Y.PC", value = "Y", paste0("PC", 1:min(nacho_object$n_comp, 5))),
    by = unique(c("CartridgeID", nacho_object$access, colour_name))
  ) %>% 
    dplyr::filter(as.numeric(gsub("PC", "", X.PC)) < as.numeric(gsub("PC", "", Y.PC))) %>% 
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = "X", 
        y = "Y", 
        colour = colour_name
      )
    ) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::stat_ellipse(na.rm = TRUE) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(Y.PC), 
        cols = ggplot2::vars(X.PC), 
        scales = "free"
      ) +
      {if (!legend) ggplot2::guides(colour = "none")}
  print(p)
  cat("\n")
  
  cat("\n\n")
  cat("#### Inertia {-}\n\n")
  p <- nacho_object$pc_sum %>% 
    dplyr::rename(ProportionofVariance = `Proportion of Variance`) %>% 
    dplyr::mutate(PoV = scales::percent(ProportionofVariance)) %>% 
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(x = "PC", y = "ProportionofVariance")
    ) +
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
  print(p)
  cat("\n")
  
  
  cat("\n\n")
  cat("## Normalisation Factors {- .tabset}\n\n")
  
  cat("\n\n")
  cat("### Positive Factor vs. Background Threshold {-}\n\n")
  p <- nacho_object$nacho %>% 
    dplyr::select(
      CartridgeID, 
      !!colour_name, 
      !!nacho_object$access, 
      Negative_factor, 
      Positive_factor
    ) %>% 
    dplyr::distinct() %>% 
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = "Negative_factor", 
        y = "Positive_factor", 
        colour = colour_name
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::labs(x = "Negative Factor", y = "Positive Factor") +
      ggplot2::scale_y_log10()
  print(p)
  cat("\n")
  
  
  cat("\n\n")
  cat("### Housekeeping Factor {-}\n\n")
  p <- nacho_object$nacho %>% 
    dplyr::select(
      CartridgeID, 
      !!colour_name, 
      !!nacho_object$access, 
      House_factor, 
      Positive_factor
    ) %>% 
    dplyr::distinct() %>% 
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = "Positive_factor", 
        y = "House_factor", 
        colour = colour_name
      )
    ) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::geom_point(na.rm = TRUE) +
      ggplot2::labs(x = "Positive Factor", y = "Houskeeping Factor") +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10()
  print(p)
  cat("\n")
  
  cat("\n\n")
  cat("### Normalisation Result {-}\n\n")
  p <- nacho_object$nacho %>% 
    dplyr::select(
      CartridgeID, 
      !!nacho_object$access, 
      Count, 
      Count_Norm, 
      Name
    ) %>% 
    dplyr::distinct() %>% 
    dplyr::filter(
      if (is.null(nacho_object$housekeeping_genes)) {
        CodeClass%in%"Positive" 
      } else {
        Name%in%nacho_object$housekeeping_genes
      }
    ) %>% 
    tidyr::gather(key = "Status", value = "Count", c("Count", "Count_Norm")) %>% 
    dplyr::mutate(
      Status = factor(
        x = c("Count" = "Raw", "Count_Norm" = "Normalised")[Status],
        levels = c("Count" = "Raw", "Count_Norm" = "Normalised")
      ),
      Count = Count + 1
    ) %>% 
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = nacho_object$access,
        y = "Count"
      )
    ) +
      ggplot2::geom_line(mapping = ggplot2::aes_string(colour = "Name", group = "Name"), na.rm = TRUE) +
      ggplot2::facet_grid(cols = ggplot2::vars(Status)) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      ggplot2::scale_x_discrete(label = NULL) +
      ggplot2::scale_y_log10(limits = c(1, NA)) +
      ggplot2::labs(
        x = "Sample Index",
        y = "Counts + 1",
        colour = if (is.null(nacho_object$housekeeping_genes)) "Positive Control" else "Housekeeping Genes"
      ) +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor.x = ggplot2::element_blank()
      ) + 
      {if (!legend |  length(nacho_object$housekeeping_genes)>10) ggplot2::guides(colour = "none")} +
      ggplot2::geom_smooth(
        mapping = ggplot2::aes(
          x = as.numeric(as.factor(get(nacho_object$access))),
          linetype = "Loess"
        ),
        colour = "black",
        se = TRUE,
        method = "loess"
      ) +
      ggplot2::labs(linetype = "Smooth")
  print(p)
  cat("\n")
  
  
  invisible()
}
