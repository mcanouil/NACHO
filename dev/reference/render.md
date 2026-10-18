# Render the quality-control report of a nacho object

Writes a quality-control report with Quarto. The report starts with a
cover that gives the study details. Then comes a decision summary, with
the flagged samples and notes on how the data were processed. A table
gives each setting and limit, with its meaning and its source. Each plot
of [`autoplot()`](https://m.canouil.dev/NACHO/dev/reference/autoplot.md)
and each table has a short "How to read this". A methods appendix gives
the citation and the package versions.

## Usage

``` r
render(
  x,
  format = c("html", "typst"),
  output_dir = ".",
  colour = "CartridgeID",
  group = NULL,
  size = 1,
  show_legend = TRUE,
  outliers_factor = 1,
  outliers_labels = NULL,
  title = NULL,
  author = NULL
)
```

## Arguments

- x:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- format:

  `"html"` (the default) for a self-contained HTML file, or `"typst"`
  for a PDF.

- output_dir:

  The directory to write `nacho-report.html` or `nacho-report.pdf` to.
  It is created when it does not exist.

- colour:

  The column of `nacho_samples(x)` that colours the samples.

- group:

  The column of `nacho_samples(x)` with the biological groups, or
  `NULL`. With a group, the report checks whether batches and groups are
  confounded; see
  [`batch_diagnostics()`](https://m.canouil.dev/NACHO/dev/reference/batch_diagnostics.md).

- size:

  The point size.

- show_legend:

  If `FALSE`, hide the colour legends.

- outliers_factor:

  The size of flagged samples, relative to `size`.

- outliers_labels:

  The column of `nacho_samples(x)` that labels the flagged samples, or
  `NULL` for no labels.

- title:

  The report title. `NULL` or empty uses "NanoString quality-control
  report". A missing value (`NA`) is an error.

- author:

  Who prepared the report, as one string, for example
  `"Jane Doe, Genomics Core"`. `NULL` or empty leaves it out. A missing
  value (`NA`) is an error.

## Value

The path of the report, invisibly.

## Details

The report needs the Quarto command-line interface 1.9.18 or newer, and
the quarto, knitr and rmarkdown packages. RStudio and Positron bundle
Quarto; elsewhere, install it from
<https://quarto.org/docs/get-started/>. The PDF goes through Typst,
which Quarto bundles, so no LaTeX is needed.

The PDF is tagged and declares PDF/UA-1. When Typst compiles the PDF, it
checks the PDF/UA-1 rules that it can enforce. When Quarto shows the
warning "verapdf is not installed", you can ignore it: it only means
that Quarto did not do the extra validation step.

## Examples

``` r
if (interactive()) {
  data(GSE74821)
  render(GSE74821, output_dir = tempdir())
  render(GSE74821, format = "typst", output_dir = tempdir())
}
```
