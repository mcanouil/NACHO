# Produce a "nacho" object from RCC NanoString files

This function is used to preprocess the data from NanoString nCounter.

## Usage

``` r
load_rcc(
  data_directory,
  ssheet_csv,
  id_colname = NULL,
  housekeeping_genes = NULL,
  housekeeping_predict = FALSE,
  housekeeping_norm = NULL,
  normalisation_method = "GEO",
  background = "none",
  background_mode = "threshold",
  ruv_k = NULL,
  instrument = NULL,
  preset = "nsolver",
  n_comp = 10
)
```

## Arguments

- data_directory:

  \[[character](https://rdrr.io/r/base/character.html)\] A character
  string of the directory where the data are stored.

- ssheet_csv:

  \[[character](https://rdrr.io/r/base/character.html)\] or
  \[[data.frame](https://rdrr.io/r/base/data.frame.html)\] Either a
  string with the name of the CSV of the samplesheet or the samplesheet
  as a `data.frame`. Should contain a column that matches the file names
  in the folder. NACHO computes the columns `Date`, `ID`, `BD`,
  `ScannerID`, `StagePosition`, `CartridgeID`, `FoV`, `PCL`, `LoD`,
  `MC`, `MedC`, `Positive_factor`, `Negative_factor`, `Detection_rate`,
  `Background`, `House_factor`, `Housekeeping_detected`,
  `Ligation_order`, `Ligation_R2`, `Ligation_NEG`, `Haemolysis`,
  `is_outlier`, `PC1`, `PC2`, ... and `W_1`, `W_2`, .... A sample sheet
  column with one of these names is replaced, with a warning.

- id_colname:

  \[[character](https://rdrr.io/r/base/character.html)\] Character
  string of the column in `ssheet_csv` that matches the file names in
  `data_directory`.

- housekeeping_genes:

  \[[character](https://rdrr.io/r/base/character.html)\] A vector of
  names of the miRNAs/mRNAs that should be used as housekeeping genes.
  Default is `NULL`.

- housekeeping_predict:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the housekeeping genes should be predicted (`TRUE`) or not
  (`FALSE`). Default is `FALSE`. Prediction picks the five most stable
  genes by geNorm on the raw counts. It is skipped for the miRNA methods
  of `normalisation_method`.

- housekeeping_norm:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the housekeeping normalisation should be performed. `NULL`
  (the default) normalises with housekeeping genes for mRNA panels that
  have them. On miRNA panels, whose housekeeping mRNAs sit at
  background, it does so only when you pass `housekeeping_genes` or set
  `housekeeping_predict = TRUE`. Pass `TRUE` or `FALSE` to override
  this. The miRNA methods of `normalisation_method` ignore it.

- normalisation_method:

  \[[character](https://rdrr.io/r/base/character.html)\] `"GEO"` (the
  default) or `"GLM"` scale samples by their positive controls, with the
  geometric mean or a Poisson model, then by the housekeeping genes;
  `"RUVg"` scales by the positive controls with the geometric mean, then
  removes `ruv_k` factors of unwanted variation estimated from the
  housekeeping genes, which
  [`nacho_samples()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md)
  returns as `W_1`, `W_2`, ... for use as covariates. For miRNA panels,
  `"stable_mirna"`, `"total_mirna"`, `"spike_in"` and `"ligation"` scale
  by the five most stable miRNAs, the miRNAs above 50 counts, the
  spike-in controls or the ligation positive controls, in the order
  Bruker recommends for plasma and serum. These methods ignore
  `housekeeping_genes`, `housekeeping_predict` and `housekeeping_norm`,
  and give their scaling factor as `House_factor`. When a GLM fit fails,
  NACHO warns and uses the geometric mean instead, as
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  describes.

- background:

  \[[character](https://rdrr.io/r/base/character.html)\] How to estimate
  each sample's background from its negative controls: `"none"` (the
  default), `"mean"`, `"mean_2sd"` (mean plus two standard deviations),
  `"median"`, `"max"` or `"geo"` (geometric mean).

- background_mode:

  \[[character](https://rdrr.io/r/base/character.html)\] What to do with
  that background: `"threshold"` (the default) raises lower counts to
  it, which Bruker recommends when fold changes matter, and `"subtract"`
  removes it and floors at 0.

- ruv_k:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] The number of
  unwanted factors RUVg removes; `NULL` uses
  [`suggest_ruv_k()`](https://m.canouil.dev/NACHO/dev/reference/suggest_ruv_k.md).
  A `ruv_k` above two less than the number of samples, or one less than
  the number of control genes, is lowered with a
  `nacho_warning_ruv_k_reduced` warning. Other methods ignore it.

- instrument:

  \[[character](https://rdrr.io/r/base/character.html)\] The nCounter
  instrument, `"max"`, `"flex"`, `"pro"` or `"sprint"`, which sets the
  binding density limits. `NULL` (the default) reads it from the RCC
  files when they say, and otherwise uses the MAX/FLEX limits with a
  warning.

- preset:

  \[[character](https://rdrr.io/r/base/character.html)\] `"nsolver"`
  (the default) or `"legacy"`; see
  [`nacho_thresholds()`](https://m.canouil.dev/NACHO/dev/reference/nacho_thresholds.md).

- n_comp:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] Number indicating
  the number of principal components to compute. Cannot be more than n-1
  samples. Default is `10`.

## Value

A `nacho` object; read its content with
[`nacho_counts()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md),
[`nacho_samples()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md),
[`nacho_probes()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md)
and
[`nacho_qc()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md).

## Examples

``` r

if (interactive()) {
  library(GEOquery)
  library(NACHO)

  # Import data from GEO
  gse <- GEOquery::getGEO(GEO = "GSE74821")
  targets <- Biobase::pData(Biobase::phenoData(gse[[1]]))
  GEOquery::getGEOSuppFiles(GEO = "GSE74821", baseDir = tempdir())
  utils::untar(
    tarfile = file.path(tempdir(), "GSE74821", "GSE74821_RAW.tar"),
    exdir = file.path(tempdir(), "GSE74821")
  )
  targets$IDFILE <- list.files(
    path = file.path(tempdir(), "GSE74821"),
    pattern = ".RCC.gz$"
  )
  targets[] <- lapply(X = targets, FUN = iconv, from = "latin1", to = "ASCII")
  utils::write.csv(
    x = targets,
    file = file.path(tempdir(), "GSE74821", "Samplesheet.csv")
  )

  # Read RCC files and format
  nacho <- load_rcc(
    data_directory = file.path(tempdir(), "GSE74821"),
    ssheet_csv = file.path(tempdir(), "GSE74821", "Samplesheet.csv"),
    id_colname = "IDFILE"
  )
}
```
