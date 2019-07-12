#' Render a HTML report from [summarise] or [normalise]
#'
#' This function create a `Rmarkdown` script and render it as a HTML document.
#' The HTML document is a quality-control report using all the metrics from [visualise]
#' based on recommendations from NanoString.
#'
#' @inheritParams normalise
#' @inheritParams plot
#' @param colour [[character]] Character string of the column in `ssheet_csv`
#'   or more generally in `nacho_object$nacho` to be used as grouping colour.
#' @param output_file [[character]] The name of the output file.
#'   If using `NULL` then the output filename will be based on filename for the input file.
#'   If a filename is provided, a path to the output file can also be provided.
#'   Note that the `output_dir` option allows for specifying the output file path as well,
#'   however, if also specifying the path, the directory must exist.
#'   If `output_file` is specified but does not have a file extension,
#'   an extension will be automatically added according to the output format.
#'   To avoid the automatic file extension, put the `output_file` value in `I()`, *e.g.*, `I('my-output')`.
#' @param output_dir [[character]] The output directory for the rendered output_file.
#'   This allows for a choice of an alternate directory to which the output file should be written
#'   (the default output directory is the working directory, *i.e.*, `getwd()`).
#'   If a path is provided with a filename in `output_file` the directory specified here will take precedence.
#'   Please note that any directory path provided will create any necessary directories if they do not exist.
#' @param show_legend [[logical]] Boolean to indicate whether the plot legends should
#'   be plotted (`TRUE`) or not (`FALSE`). Default is `TRUE`.
#' @param keep_rmd [[logical]] Boolean to indicate whether the Rmd file used to produce the HTML report
#'   is copied to the directory provided in `output_dir`. Default is `FALSE`.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#'
#' data(GSE74821)
#' render(nacho_object = GSE74821, output_dir = NULL)
#'
render <- function(
  nacho_object,
  colour = "CartridgeID",
  output_file = "NACHO_QC.html",
  output_dir = getwd(),
  size = 0.5,
  show_legend = TRUE,
  keep_rmd = FALSE
) {
  temp_file <- gsub("\\\\", "/", tempfile())

  if (is.null(output_dir)) output_dir <- dirname(temp_file)

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

  nacho_hex <- system.file("help", "figures", "nacho_hex.png", package = "NACHO")
  if (!file.exists(nacho_hex)) {
    nacho_hex <- grep("figures", list.files(
      path = system.file(package = "NACHO"),
      pattern = "nacho_hex.png",
      recursive = TRUE,
      full.names = TRUE
    ), value = TRUE)
  }

  cat(
    '\n<center>[![](', nacho_hex, '){width=150px}](https://mcanouil.github.io/NACHO)</center>',
    file = temp_file_rmd,
    append = TRUE,
    sep = ""
  )

  save(list = "nacho_object", file = temp_file_data)

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
    '  autodep = TRUE,',
    '  fig.align = "center"',
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
    paste0('  colour = "', colour, '",'),
    paste0('  size = ', size, ','),
    paste0('  show_legend = ', show_legend),
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

  if (keep_rmd) {
    file.copy(
      from = temp_file_rmd,
      to = file.path(output_dir, gsub(".html", ".Rmd", output_file))
    )
  }

  rmarkdown::render(
    input = temp_file_rmd,
    output_file = output_file,
    output_dir = output_dir,
    encoding = "UTF-8",
    quiet = TRUE
  )

  unlink(temp_file_data)
  unlink(temp_file_rmd)
  invisible()
}


#' print_nacho
#'
#' This function allows to print text and figures from the results of a call to `summarise()`
#' or `normalise()`.
#' It is intended to be used in a `Rmarkdown` chunk.
#'
#' @inheritParams render
#' @inheritParams plot
#'
#' @return NULL
#' @export
#' @rdname render
#'
#' @examples
#'
#' data(GSE74821)
#' print_nacho(GSE74821, colour = "CartridgeID", size = 0.5, show_legend = TRUE)
#'
print_nacho <- function(nacho_object, colour = "CartridgeID", size = 0.5, show_legend = FALSE) {
  if (is.numeric(nacho_object$nacho[[colour]])) {
    nacho_object$nacho[[colour]] <- as.character(nacho_object$nacho[[colour]])
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
    "BD" = "Binding Density",
    "FoV" = "Field of View (Imaging)",
    "PC" = "Positive Control linearity",
    "LoD" = "Limit of Detection"
  )
  units <- c(
    "BD" = '"(Optical features / ", mu, m^2, ")"',
    "FoV" = '"(% Counted)"',
    "PC" = '(R^2)',
    "LoD" = '"(Z)"'
  )
  details <- c(
    "BD" = paste(
      "The imaging unit only counts the codes that are unambiguously distinguishable.",
      "It simply will not count codes that overlap within an image.",
      "This provides increased confidence that the molecular counts you receive are from truly recognisable codes.",
      "Under most conditions, forgoing the few barcodes that do overlap will not impact your data.",
      "Too many overlapping codes in the image, however, will create a condition called image saturation in which significant data loss could occur (critical data loss from saturation is uncommon).",
      "\n",
      "To determine the level of image saturation, the nCounter instrument calculates the number of optical features per square micron for each lane as it processes the images.",
      "This is called the **Binding Density**.",
      "The **Binding Density** is useful for determining whether data collection has been compromised due to image saturation.",
      "The acceptable range for **Binding Density** is:",
      "\n",
      "* `0.1 - 2.25` for **MAX**/**FLEX** instruments",
      "* `0.1 - 1.8` for **SPRINT** instruments",
      "\n",
      "Within these ranges, relatively few reporters on the slide surface will overlap, enabling the instrument to accurately tabulate counts for each reporter species.",
      "A **Binding Density** significantly greater than the upper limit in either range is indicative of overlapping reporters on the slide surface.",
      "The counts observed in lanes with a **Binding Density** at this level may have had significant numbers of codes ignored, which could potentially affect quantification and linearity of the assay.",
      "Some of the factors that may contribute to increased **Binding Density** are listed in the Factors affecting **Binding Density** box.",
      sep = "\n"
    ),
    "FoV" = paste(
      "Each individual lane scanned on an nCounter system is divided into a few hundred imaging sections, called Fields of View (**FOV**), the exact number of which will depend on the system being used (*i.e.*, **MAX/FLEX** or **SPRINT**), and the scanner settings selected by the user.",
      "The system images these FOVs separately, and sums the barcode counts of all **FOV**s from a single lane to form the final raw data count for each unique barcode target.",
      "Finally, the system reports the number of **FOV**s successfully imaged as FOV Counted.",
      "\n",
      "Significant discrepancy between the number of **FOV** for which imaging was attempted (**FOV Count**) and for which imaging was successful (**FOV Counted**) may indicate an issue with imaging performance.",
      "Recommended percentage of registered FOVs (*i.e.*, **FOV Counted** over **FOV Count**) is `75 %`.",
      "Lanes will be flagged if this percentage is lower.",
      sep = "\n"
    ),
    "PC" = paste(
      "Six synthetic DNA control targets are included with every nCounter Gene Expression assay.",
      "Their concentrations range linearly from `128 fM` to `0.125 fM`, and they are referred to as **POS_A** to **POS_F**, respectively.",
      "These **Positive Controls** are typically used to measure the efficiency of the hybridization reaction, and their step-wise concentrations also make them useful in checking the linearity performance of the assay.",
      "An R2 value is calculated from the regression between the known concentration of each of the **Positive Controls** and the resulting counts from them (this calculation is performed using log2-transformed values).",
      "\n",
      "Since the known concentrations of the **Positive Controls** increase in a linear fashion, the resulting counts should, as well.",
      "Therefore, R2 values should be higher than `0.95`.",
      "\n",
      "Note that because POS_F has a known concentration of `0.125 fM`, which is considered below the limit of detection of the system, it should be excluded from this calculation (although you will see that **POS_F** counts are significantly higher than the negative control counts in most cases).",
      sep = "\n"
    ),
    "LoD" = paste(
      "The limit of detection is determined by measuring the ability to detect **POS_E**, the `0.5 fM` positive control probe, which corresponds to about 10,000 copies of this target within each sample tube.",
      "On a **FLEX**/**MAX** system, the standard input of `100 ng` of total RNA will roughly correspond to about 10,000 cell equivalents (assuming one cell contains `10 pg` total RNA on average).",
      "An nCounter assay run on the **FLEX**/**MAX** system should thus conservatively be able to detect roughly one transcript copy per cell for each target (or 10,000 total transcript copies).",
      "In most assays, you will observe that even the **POS_F** probe (equivalent to 0.25 copies per cell) is detectable above background.",
      "\n",
      "To determine whether **POS_E** is detectable, it can be compared to the counts for the negative control probes.",
      "Every nCounter Gene Expression assay is manufactured with eight negative control probes that should not hybridize to any targets within the sample.",
      "Counts from these will approximate general non-specific binding of probes within the samples being run.",
      "The counts of **POS_E** should be higher than two times the standard deviation above the mean of the negative control.",
      sep = "\n"
    )
  )

  metrics <- switch(
    EXPR = attr(nacho_object, "RCC_type"),
    "n1" = c("BD", "FoV", "PC", "LoD"),
    "n8" = c("BD", "FoV")
  )

  for (imetric in metrics) {
    cat("\n\n##", labels[imetric], "\n\n")
    cat(details[imetric], "\n")
    print(plot(
      x = imetric,
      nacho_object = nacho_object,
      colour = colour,
      size = size,
      show_legend = show_legend
    ))
    cat("\n")
  }

  cat("\n\n# Control Genes\n\n")
  for (icodeclass in c("Positive", "Negative", "Housekeeping")) {
    cat("\n\n")
    cat("##", icodeclass, "\n\n")
    print(plot(
      x = imetric,
      nacho_object = nacho_object,
      colour = colour,
      size = size,
      show_legend = show_legend
    ))
    cat("\n")
  }

  cat("\n\n## Control Probe Expression\n\n")
  print(plot(
    x = "PN",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n# QC Visuals\n\n")

  cat("\n\n## Average Count vs. Binding Density\n\n")
  print(plot(
    x = "ACBD",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n## Average Count vs. Median Count\n\n")
  print(plot(
    x = "ACMC",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n## Principal Component\n\n")
  cat("\n\n### PC1 vs. PC2\n\n")
  print(plot(
    x = "PCA12",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n### Factorial planes\n\n")
  print(plot(
    x = "PCA",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n### Inertia\n\n")
  print(plot(
    x = "PCAi",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n# Normalisation Factors\n\n")

  cat("\n\n## Positive Factor vs. Background Threshold\n\n")
 print(plot(
    x = "PFNF",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n## Housekeeping Factor\n\n")
  print(plot(
    x = "HF",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  cat("\n\n## Normalisation Result\n\n")
  print(plot(
    x = "NORM",
    nacho_object = nacho_object,
    colour = colour,
    size = size,
    show_legend = show_legend
  ))
  cat("\n")

  invisible()
}
