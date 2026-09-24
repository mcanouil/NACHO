# Render an HTML report of a "nacho" object

This function creates an R Markdown script and renders it as an HTML
document. The HTML document is a quality-control report using all the
metrics from
[`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
based on recommendations from NanoString.

## Usage

``` r
render(
  nacho_object,
  colour = "CartridgeID",
  output_file = "NACHO_QC.html",
  output_dir = ".",
  size = 1,
  show_legend = TRUE,
  show_outliers = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  clean = TRUE
)
```

## Arguments

- nacho_object:

  \[[list](https://rdrr.io/r/base/list.html)\] A list object of class
  `"nacho"` obtained from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- colour:

  \[[character](https://rdrr.io/r/base/character.html)\] Character
  string of the column in `ssheet_csv` or more generally in
  `nacho_object$nacho` to be used as grouping colour.

- output_file:

  \[[character](https://rdrr.io/r/base/character.html)\] The name of the
  output file.

- output_dir:

  \[[character](https://rdrr.io/r/base/character.html)\] The output
  directory for the rendered output_file. This allows for a choice of an
  alternate directory to which the output file should be written (the
  default output directory is the working directory, *i.e.*, `.`). If a
  path is provided with a filename in `output_file` the directory
  specified here will take precedence. Please note that any directory
  path provided will create any necessary directories if they do not
  exist.

- size:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] A numeric
  controlling point size
  ([`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  or line width
  ([`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html)).

- show_legend:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the plot legends should be plotted (`TRUE`) or not (`FALSE`).
  Default is `TRUE`.

- show_outliers:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the outliers should be highlighted in red (`TRUE`) or not
  (`FALSE`). Default is `TRUE`.

- outliers_factor:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] Size factor for
  outliers compared to `size`. Default is `1`.

- outliers_labels:

  \[[character](https://rdrr.io/r/base/character.html)\] Character to
  indicate which column in `nacho_object$nacho` should be used to be
  printed as the labels for outliers or not. Default is `NULL`.

- clean:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the Rmd and RData files used to produce the HTML report are
  removed from `output_dir`. Default is `TRUE`.

## Examples

``` r

if (interactive()) {
  data(GSE74821)
  render(GSE74821)
}
```
