# Package index

## Load and normalise

Read RCC files, compute quality-control metrics and normalise counts.

- [`load_rcc()`](https://m.canouil.dev/NACHO/reference/load_rcc.md) :
  Produce a "nacho" object from RCC NanoString files
- [`read_nacho()`](https://m.canouil.dev/NACHO/reference/read_nacho.md)
  : Read a saved NACHO object
- [`upgrade_nacho()`](https://m.canouil.dev/NACHO/reference/upgrade_nacho.md)
  : Convert a NACHO 2 object to NACHO 3
- [`as_summarized_experiment()`](https://m.canouil.dev/NACHO/reference/as_summarized_experiment.md)
  [`as_nacho()`](https://m.canouil.dev/NACHO/reference/as_summarized_experiment.md)
  : Convert between nacho objects and Bioconductor containers
- [`nacho_thresholds()`](https://m.canouil.dev/NACHO/reference/nacho_thresholds.md)
  : Quality-control thresholds
- [`write_thresholds()`](https://m.canouil.dev/NACHO/reference/write_thresholds.md)
  [`read_thresholds()`](https://m.canouil.dev/NACHO/reference/write_thresholds.md)
  : Write and read quality-control thresholds
- [`normalise()`](https://m.canouil.dev/NACHO/reference/normalise.md) :
  (Re)normalise a nacho object
- [`exclude_outliers()`](https://m.canouil.dev/NACHO/reference/exclude_outliers.md)
  : Drop outliers and normalise the other samples again
- [`housekeeping_stability()`](https://m.canouil.dev/NACHO/reference/housekeeping_stability.md)
  : Rank genes by expression stability
- [`suggest_ruv_k()`](https://m.canouil.dev/NACHO/reference/suggest_ruv_k.md)
  : Suggest how many unwanted factors RUVg should remove
- [`batch_diagnostics()`](https://m.canouil.dev/NACHO/reference/batch_diagnostics.md)
  : Batch and confounding diagnostics

## Visualise and report

Explore quality control interactively, plot a single metric, or write a
full report.

- [`visualise()`](https://m.canouil.dev/NACHO/reference/visualise.md) :
  Explore and tune quality control in the NACHO app
- [`autoplot.nacho`](https://m.canouil.dev/NACHO/reference/autoplot.nacho.md)
  : Plot the quality control of a nacho object
- [`nacho-methods`](https://m.canouil.dev/NACHO/reference/nacho-methods.md)
  : Basic methods for a nacho object
- [`nacho_counts()`](https://m.canouil.dev/NACHO/reference/nacho-accessors.md)
  [`nacho_samples()`](https://m.canouil.dev/NACHO/reference/nacho-accessors.md)
  [`nacho_probes()`](https://m.canouil.dev/NACHO/reference/nacho-accessors.md)
  [`nacho_qc()`](https://m.canouil.dev/NACHO/reference/nacho-accessors.md)
  : Get the content of a nacho object
- [`filter_detected()`](https://m.canouil.dev/NACHO/reference/filter_detected.md)
  : Keep genes detected in enough samples
- [`render()`](https://m.canouil.dev/NACHO/reference/render.md) : Render
  the quality-control report of a nacho object

## Shiny application

- [`nacho_app()`](https://m.canouil.dev/NACHO/reference/nacho_app.md) :
  Run the NACHO app
- [`deploy()`](https://m.canouil.dev/NACHO/reference/deploy.md) : Deploy
  (copy) the Shiny application to the specified directory

## Data

- [`GSE74821`](https://m.canouil.dev/NACHO/reference/GSE74821.md) : A
  "nacho" object with 48 samples from the GSE74821 dataset
