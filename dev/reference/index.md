# Package index

## Load and normalise

Read RCC files, compute quality-control metrics and normalise counts.

- [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  : Produce a "nacho" object from RCC NanoString files
- [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  : (re)Normalise a "nacho" object
- [`check_outliers()`](https://m.canouil.dev/NACHO/dev/reference/check_outliers.md)
  : Annotate a "nacho" object for outliers

## Visualise and report

Explore quality control interactively, plot a single metric, or write a
full report.

- [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
  : Visualise quality-control metrics of a "nacho" object
- [`autoplot(`*`<nacho>`*`)`](https://m.canouil.dev/NACHO/dev/reference/autoplot.nacho.md)
  : Plot quality-control metrics and thresholds of a "nacho" object
- [`print(`*`<nacho>`*`)`](https://m.canouil.dev/NACHO/dev/reference/print.nacho.md)
  : Print method for "nacho" object
- [`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md) :
  Render an HTML report of a "nacho" object

## Shiny application

- [`deploy()`](https://m.canouil.dev/NACHO/dev/reference/deploy.md) :
  Deploy (copy) the Shiny application to the specified directory

## Data

- [`GSE74821`](https://m.canouil.dev/NACHO/dev/reference/GSE74821.md) :
  A "nacho" object with 48 samples from the GSE74821 dataset
