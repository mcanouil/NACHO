# Print method for "nacho" object

This function prints text and figures from the results of a call to
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md) or
[`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).
It is intended to be used in an R Markdown chunk.

## Usage

``` r
# S3 method for class 'nacho'
print(
  x,
  colour = "CartridgeID",
  size = 0.5,
  show_legend = FALSE,
  show_outliers = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  echo = FALSE,
  title_level = 1,
  xaringan = FALSE,
  ...
)
```

## Arguments

- x:

  \[[list](https://rdrr.io/r/base/list.html)\] A list object of class
  `"nacho"` obtained from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

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

- echo:

  \[[logical](https://rdrr.io/r/base/logical.html)\] A boolean to
  indicate whether text and plots should be printed. Mainly for use
  within an R Markdown chunk.

- title_level:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] A numeric to
  indicate the title level to start with, using markdown style, *i.e.*,
  the number of `"#"`.

- xaringan:

  \[[logical](https://rdrr.io/r/base/logical.html)\] A boolean to format
  output for xaringan slides.

- ...:

  Other arguments (*Not used*).

## Examples

``` r

data(GSE74821)
print(GSE74821)
#> List of 11
#>  $ access              : chr "IDFILE"
#>  $ housekeeping_genes  : chr [1:8] "MRPL19" "PSMC4" "SF3A1" "RPLP0" ...
#>  $ housekeeping_predict: logi FALSE
#>  $ housekeeping_norm   : logi TRUE
#>  $ normalisation_method: chr "GLM"
#>  $ remove_outliers     : logi FALSE
#>  $ n_comp              : num 10
#>  $ data_directory      : chr "~/"
#>  $ pc_sum              :'data.frame':    10 obs. of  4 variables:
#>  $ nacho               :Classes ‘data.table’ and 'data.frame':   3456 obs. of  86 variables:
#>   ..- attr(*, "sorted")= chr "IDFILE"
#>   ..- attr(*, ".internal.selfref")=<pointer: (nil)> 
#>  $ outliers_thresholds :List of 6
#>  - attr(*, "RCC_type")= chr "n1"
#>  - attr(*, "class")= chr "nacho"
```
