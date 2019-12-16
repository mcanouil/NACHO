#' Render a HTML report from [summarise] or [normalise]
#'
#' This function create a `Rmarkdown` script and render it as a HTML document.
#' The HTML document is a quality-control report using all the metrics from [visualise]
#' based on recommendations from NanoString.
#'
#' @inheritParams normalise
#' @param colour [[character]] Character string of the column in `ssheet_csv`
#'   or more generally in `nacho_object$nacho` to be used as grouping colour.
#' @param output_file [[character]] The name of the output file.
#' @param output_dir [[character]] The output directory for the rendered output_file.
#'   This allows for a choice of an alternate directory to which the output file should be written
#'   (the default output directory is the working directory, *i.e.*, `.`).
#'   If a path is provided with a filename in `output_file` the directory specified here will take precedence.
#'   Please note that any directory path provided will create any necessary directories if they do not exist.
#' @param size [[numeric]] A `numeric` controlling point size ([geom_point] or [geom_beeswarm])
#'   or line size ([geom_line]).
#' @param show_legend [[logical]] Boolean to indicate whether the plot legends should
#'   be plotted (`TRUE`) or not (`FALSE`). Default is `TRUE`.
#' @param show_outliers [[logical]] Boolean to indicate whether the outliers should be highlighted
#'   in red (`TRUE`) or not (`FALSE`). Default is `TRUE`.
#' @param outliers_factor [[numeric]] Size factor for outliers compared to `size`. Default is `1`.
#' @param outliers_labels [[logical]] Boolean to indicate whether the labels for outliers should be printed
#'   in red (`TRUE`) or not (`FALSE`). Default is `FALSE`.
#' @param clean [[logical]] Boolean to indicate whether the Rmd and Rdata file used to produce the HTML report
#'   are removed from `output_dir`. Default is `TRUE`.
#'
#' @return NULL
#' @importFrom knitr opts_chunk
#' @importFrom sessioninfo session_info
#' @export
#'
#' @examples
#'
#' if (interactive()) {
#'   data(GSE74821)
#'   render(GSE74821)
#' }
#'
render <- function(
  nacho_object,
  colour = "CartridgeID",
  output_file = "NACHO_QC.html",
  output_dir = ".",
  size = 1,
  show_legend = TRUE,
  show_outliers = TRUE,
  outliers_factor = 1,
  outliers_labels = FALSE,
  clean = TRUE
) {
  if (missing(nacho_object) {
    stop(
      '[NACHO] "nacho_object" is missing, results from "summarise()" and/or "normalise()" is mandatory!'
    )
  }
  if (!attr(nacho_object, "RCC_type") %in% c("n1", "n8")) {
    stop('[NACHO] RCC type must be either "n1" or "n8"!')
  }
  temp_dir <- file.path(normalizePath(output_dir), "tmp_nacho")
  dir.create(temp_dir, showWarnings = FALSE)
  temp_file <- file.path(temp_dir, gsub("\\.[^.]+$", ".Rmd", output_file))

  cat(
    '---',
    'title: "NanoString Quality-Control Report"',
    # 'author: "[NACHO](https://mcanouil.github.io/NACHO)"',
    'params:',
    '  nacho_object: NULL',
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
    # 'library(NACHO)',
    '```',
    '\n',
    '```{r logo, out.width = 150}',
    'knitr::include_graphics(',
    '  grep(',
    '    pattern = file.path("figures", "nacho_hex.png"),',
    '      x = list.files(',
    '      path = system.file(package = "NACHO"),',
    '      recursive = TRUE,',
    '      full.names = TRUE',
    '    ), value = TRUE',
    '  )',
    ')',
    '```',
    '\n',
    '```{r nacho_qc}',
    'print.nacho(',
    '  x = params[["nacho_object"]],',
    paste0('  colour = "', colour, '",'),
    paste0('  size = ', size, ','),
    paste0('  show_legend = ', show_legend, ','),
    paste0('  show_outliers = ', show_outliers, ','),
    paste0('  outliers_factor = ', outliers_factor, ','),
    paste0('  outliers_labels = ', outliers_labels, ','),
    '  echo = TRUE',
    ')',
    '```',
    '\n\n',
    '# R session information\n',
    '```{r session_info, results = "markup"}',
    'options("width" = 110)',
    'sessioninfo::session_info()',
    '```',
    sep = "\n",
    file = temp_file,
    append = FALSE
  )

  rmarkdown::render(
    input = temp_file,
    output_file = output_file,
    output_dir = output_dir,
    encoding = "UTF-8",
    quiet = TRUE,
    params = list(nacho_object = nacho_object)
  )

  if (clean) {
    unlink(file.path(output_dir, "tmp_nacho"), recursive = TRUE)
  }

  invisible()
}
