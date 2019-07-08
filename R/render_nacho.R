#' render_nacho
#'
#' @inheritParams normalise
#' @param colour_name [character] Character string of the column in \code{ssheet_csv}
#'   or more generally in \code{nacho_object$nacho}.
#' @param output_file [character] The name of the output file.
#'   If using `NULL` then the output filename will be based on filename for the input file.
#'   If a filename is provided, a path to the output file can also be provided.
#'   Note that the `output_dir` option allows for specifying the output file path as well,
#'   however, if also specifying the path, the directory must exist.
#'   If `output_file` is specified but does not have a file extension,
#'   an extension will be automatically added according to the output format.
#'   To avoid the automatic file extension, put the output_file value in `I()`, e.g., I('my-output').
#' @param output_dir [character] The output directory for the rendered output_file.
#'   This allows for a choice of an alternate directory to which the output file should be written
#'   (the default output directory is the working directory).
#'   If a path is provided with a filename in `output_file` the directory specified here will take precedence.
#'   Please note that any directory path provided will create any necessary directories if they do not exist.
#' @param legend [logical] Boolean to indicate whether the plot legends should
#'   be plotted (\code{TRUE}) or not (\code{FALSE}). Default is \code{FALSE}.
#' @param keep_rmd [logical] Boolean to indicate whether the Rmd file used to produce the HTML report
#'   is copied to the directory provided in `output_dir`
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
#'
render_nacho <- function(
  nacho_object,
  colour_name = "CartridgeID",
  output_file = "NACHO_QC.html",
  output_dir = NULL,
  legend = FALSE,
  keep_rmd = FALSE
) {
  if (is.null(output_dir)) output_dir <- getwd()

  temp_file <- tempfile()

  temp_file_rmd <- paste0(temp_file, ".Rmd")
  temp_file_data <- paste0(temp_file, ".Rdata")

  cat(
    '---',
    'title: "NanoString Quality-Control Report"',
    # 'author: "[NACHO](https://mcanouil.github.io/NACHO)"',
    'output:',
    '  html_document:',
    '    theme: simplex',
    '    toc: true',
    '    toc_depth: 2',
    '    toc_float:' ,
    '      collapsed: false',
    '    fig_width: 6.3',
    '    fig_height: 4.7',
    '    number_sections: true',
    '    self_contained: true',
    '    mathjax: default',
    '    df_print: kable',
    '---',
    sep = "\n",
    file = temp_file_rmd,
    append = FALSE
  )

  cat(
    '\n',
    paste0(
      '<a href = "https://mcanouil.github.io/NACHO"><center>![](',
      system.file("help", "figures", "nacho_hex.png", package = "NACHO"),
      '){width=150px}</center></a>'
    ),
    file = temp_file_rmd,
    append = TRUE
  )

  save(list = c("nacho_object", "colour_name", "legend"), file = temp_file_data)

  cat(
    '\n',
    '```{r setup, include = FALSE}',
    'options(stringsAsFactors = FALSE)',
    'knitr::opts_chunk$set(',
    '  results = "asis",',
    '  include = TRUE,',
    '  echo = FALSE,',
    '  warning = FALSE,',
    '  message = FALSE,',
    '  tidy = FALSE,',
    '  crop = TRUE,',
    '  autodep = TRUE',
    ')',
    '```',
    sep = "\n",
    file = temp_file_rmd,
    append = TRUE
  )

  cat(
    '\n',
    '```{r nacho_qc}',
    'nacho_env <- new.env()',
    paste0('load("', temp_file_data, '", envir = nacho_env)'),
    'print_nacho(',
    '  nacho_object = nacho_env[["nacho_object"]],',
    '  colour_name = nacho_env[["colour_name"]],',
    '  legend = nacho_env[["legend"]]',
    ')',
    '```',
    sep = "\n",
    file = temp_file_rmd,
    append = TRUE
  )

  cat(
    "\n\n# R session information\n",
    '```{r session_info, results = "markup"}',
    'options("width" = 110)',
    'sessioninfo::session_info()',
    '```',
    sep = "\n",
    file = temp_file_rmd,
    append = TRUE
  )

  if (keep_rmd) file.copy(from = temp_file_rmd, to = paste0(output_dir, "/", gsub(".html", ".Rmd", output_file)))

  rmarkdown::render(
    input = temp_file_rmd,
    output_file = output_file,
    output_dir = output_dir,
    encoding = 'UTF-8'
  )

  unlink(temp_file_data)
  unlink(temp_file_rmd)
  invisible()
}


#' print_nacho
#'
#' @inheritParams render_nacho
#'
#' @keywords internal
#'
#' @return NULL
print_nacho <- function(nacho_object, colour_name = "CartridgeID", legend = FALSE) {
  if (is.numeric(nacho_object$nacho[[colour_name]])) {
    nacho_object$nacho[[colour_name]] <- as.character(nacho_object$nacho[[colour_name]])
  }

  cat("\n\n# RCC Summary\n\n")
  cat('  - Samples:', length(unique(nacho_object$nacho[[nacho_object$access]])), "\n")
  genes <- table(nacho_object$nacho[["CodeClass"]]) /
    length(unique(nacho_object$nacho[[nacho_object$access]]))
  cat(paste0("  - ", names(genes), ": ", genes, "\n"))

  cat("\n\n# Settings\n\n")
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

  cat("\n\n# QC Metrics\n\n")
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

  metrics <- switch(
    EXPR = attr(nacho_object, "RCC_type"),
    "n1" = c("BD", "FoV", "PC", "LoD"),
    "n8" = c("BD", "FoV")
  )

  for (imetric in metrics) {
    cat("\n\n##", labels[imetric], "\n\n")
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
      ggbeeswarm::geom_quasirandom(size = 0.5, width = 0.25, na.rm = TRUE, groupOnX = TRUE) +
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

  cat("\n\n# Control Genes\n\n")
  for (icodeclass in c("Positive", "Negative", "Housekeeping")) {
    cat("\n\n")
    cat("##", icodeclass, "\n\n")
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
      ggbeeswarm::geom_quasirandom(size = 0.5, width = 0.25, na.rm = TRUE, groupOnX = TRUE) +
      ggplot2::scale_y_log10(limits = c(1, NA)) +
      ggplot2::labs(
        x = if (icodeclass%in%c("Negative", "Positive")) "Control Name" else "Gene Name",
        y = "Counts + 1"
      ) +
      {if (!legend) ggplot2::guides(colour = "none")} +
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

  cat("\n\n## Control Probe Expression\n\n")
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

  cat("\n\n# QC Visuals\n\n")

  cat("\n\n## Average Count vs. Binding Density\n\n")
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
      ggplot2::geom_point(size = 0.5, na.rm = TRUE) +
      ggplot2::labs(
        x = labels["MC"],
        y = parse(text = paste0('paste("', labels["BD"], '", " ", ',  units["BD"], ")"))
      )
  print(p)
  cat("\n")

  cat("\n\n## Average Count vs. Median Count\n\n")
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
      ggplot2::geom_point(size = 0.5, na.rm = TRUE) +
      ggplot2::labs(
        x = labels["MC"],
        y = labels["MedC"]
      )
  print(p)
  cat("\n")

  cat("\n\n## Principal Component\n\n")
  cat("\n\n### PC1 vs. PC2\n\n")
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
      ggplot2::geom_point(size = 0.5, na.rm = TRUE) +
      ggplot2::stat_ellipse(na.rm = TRUE) +
      ggplot2::scale_colour_viridis_d(option = "plasma", direction = -1, end = 0.9) +
      {if (!legend) ggplot2::guides(colour = "none")}
  print(p)
  cat("\n")

  cat("\n\n### Factorial planes\n\n")
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
      ggplot2::geom_point(size = 0.5, na.rm = TRUE) +
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

  cat("\n\n### Inertia\n\n")
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

  cat("\n\n# Normalisation Factors\n\n")

  cat("\n\n## Positive Factor vs. Background Threshold\n\n")
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
      ggplot2::geom_point(size = 0.5, na.rm = TRUE) +
      ggplot2::labs(x = "Negative Factor", y = "Positive Factor") +
      ggplot2::scale_y_log10()
  print(p)
  cat("\n")

  cat("\n\n## Housekeeping Factor\n\n")
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
      ggplot2::geom_point(size = 0.5, na.rm = TRUE) +
      ggplot2::labs(x = "Positive Factor", y = "Houskeeping Factor") +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10()
  print(p)
  cat("\n")

  cat("\n\n## Normalisation Result\n\n")
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
      ggplot2::geom_line(mapping = ggplot2::aes_string(colour = "Name", group = "Name"), size = 0.1, na.rm = TRUE) +
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
