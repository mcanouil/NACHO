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
#'   If using `NULL` then the output filename will be based on filename for the input file.
#'   If a filename is provided, a path to the output file can also be provided.
#'   Note that the `output_dir` option allows for specifying the output file path as well,
#'   however, if also specifying the path, the directory must exist.
#'   If `output_file` is specified but does not have a file extension,
#'   an extension will be automatically added according to the output format.
#'   To avoid the automatic file extension, put the `output_file` value in `I()`, *e.g.*, `I('my-output')`.
#' @param output_dir [[character]] The output directory for the rendered output_file.
#'   This allows for a choice of an alternate directory to which the output file should be written
#'   (the default output directory is the working directory, *i.e.*, `.`).
#'   If a path is provided with a filename in `output_file` the directory specified here will take precedence.
#'   Please note that any directory path provided will create any necessary directories if they do not exist.
#' @param size [[numeric]] A `numeric` controlling point size ([geom_point] or [geom_beeswarm])
#'   or line size ([geom_line]).
#' @param show_legend [[logical]] Boolean to indicate whether the plot legends should
#'   be plotted (`TRUE`) or not (`FALSE`). Default is `TRUE`.
#' @param clean [[logical]] Boolean to indicate whether the Rmd and Rdata file used to produce the HTML report
#'   are removed from `output_dir`. Default is `TRUE`.
#'
#' @return NULL
#' @export
#'
#' @examples
#'
#' data(GSE74821)
#' render(GSE74821)
#'
render <- function(
  nacho_object,
  colour = "CartridgeID",
  output_file = "NACHO_QC.html",
  output_dir = ".",
  size = 0.5,
  show_legend = TRUE,
  clean = TRUE
) {
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
    sep = "\n",
    file = temp_file,
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
    file = temp_file,
    append = TRUE,
    sep = ""
  )

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
    file = temp_file,
    append = TRUE
  )

  cat(
    '\n',
    '```{r nacho_qc}',
    'print.nacho(',
    '  x = params[["nacho_object"]],',
    paste0('  colour = "', colour, '",'),
    paste0('  size = ', size, ','),
    paste0('  show_legend = ', show_legend),
    ')',
    '```',
    sep = "\n",
    file = temp_file,
    append = TRUE
  )

  cat(
    "\n\n# R session information\n",
    '```{r session_info, results = "markup"}',
    'options("width" = 110)',
    'sessioninfo::session_info()',
    '```',
    sep = "\n",
    file = temp_file,
    append = TRUE
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
