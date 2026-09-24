# Plot quality-control metrics and thresholds of a "nacho" object

This function plots any of the quality-control figures available within
the Shiny app using
[`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
or in the HTML report from
[`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md).

## Usage

``` r
# S3 method for class 'nacho'
autoplot(
  object,
  x,
  colour = "CartridgeID",
  size = 0.5,
  show_legend = TRUE,
  show_outliers = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  ...
)
```

## Arguments

- object:

  \[[list](https://rdrr.io/r/base/list.html)\] List obtained from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- x:

  \[[character](https://rdrr.io/r/base/character.html)\] Character
  string naming the quality-control metrics to plot from `nacho_object`.
  The possible values are:

  - `"BD"` (Binding Density)

  - `"FoV"` (Imaging)

  - `"PCL"` (Positive Control Linearity)

  - `"LoD"` (Limit of Detection)

  - `"Positive"` (Positive Controls)

  - `"Negative"` (Negative Controls)

  - `"Housekeeping"` (Housekeeping Genes)

  - `"PN"` (Positive Controls vs. Negative Controls)

  - `"ACBD"` (Average Counts vs. Binding Density)

  - `"ACMC"` (Average Counts vs. Median Counts)

  - `"PCA12"` (Principal Component 1 vs. 2)

  - `"PCAi"` (Principal Component scree plot)

  - `"PCA"` (Principal Components planes)

  - `"PFNF"` (Positive Factor vs. Negative Factor)

  - `"HF"` (Housekeeping Factor)

  - `"NORM"` (Normalisation Factor)

- colour:

  \[[character](https://rdrr.io/r/base/character.html)\] Character
  string of the column in `ssheet_csv` or more generally in
  `nacho_object$nacho` to be used as grouping colour.

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

- ...:

  Other arguments (Not used).

## Examples

``` r

data(GSE74821)

autoplot(GSE74821, x = "BD")

```
