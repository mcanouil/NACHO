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
  housekeeping_norm = TRUE,
  normalisation_method = "GEO",
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
  in the folder.

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
  (`FALSE`). Default is `FALSE`.

- housekeeping_norm:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the housekeeping normalisation should be performed. Default is
  `TRUE`.

- normalisation_method:

  \[[character](https://rdrr.io/r/base/character.html)\] Either `"GEO"`
  or `"GLM"`. Character string to indicate normalisation using the
  geometric mean (`"GEO"`) or a generalized linear model (`"GLM"`).
  Default is `"GEO"`.

- n_comp:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] Number indicating
  the number of principal components to compute. Cannot be more than n-1
  samples. Default is `10`.

## Value

\[[list](https://rdrr.io/r/base/list.html)\] A list object of class
`"nacho"`:

- `access`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  `load_rcc()` in `id_colname`.

- `housekeeping_genes`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  `load_rcc()`.

- `housekeeping_predict`:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Value passed to
  `load_rcc()`.

- `housekeeping_norm`:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Value passed to
  `load_rcc()`.

- `normalisation_method`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  `load_rcc()`.

- `remove_outliers`:

  \[[logical](https://rdrr.io/r/base/logical.html)\] `FALSE`.

- `n_comp`:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] Value passed to
  `load_rcc()`.

- `data_directory`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  `load_rcc()`.

- `pc_sum`:

  \[[data.frame](https://rdrr.io/r/base/data.frame.html)\] A
  `data.frame` with `n_comp` rows and four columns: "Standard
  deviation", "Proportion of Variance", "Cumulative Proportion" and
  "PC".

- `nacho`:

  \[[data.frame](https://rdrr.io/r/base/data.frame.html)\] A
  `data.frame` with all columns from the sample sheet `ssheet_csv` and
  all computed columns, *i.e.*, quality-control metrics and counts, with
  one sample per row.

- `outliers_thresholds`:

  \[[list](https://rdrr.io/r/base/list.html)\] A `list` of the (default)
  quality-control thresholds used.

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
