# (re)Normalise a "nacho" object

This function creates a list in which your settings, the raw counts and
normalised counts are stored, using the result from a call to
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md).

## Usage

``` r
normalise(
  nacho_object,
  housekeeping_genes = nacho_object[["housekeeping_genes"]],
  housekeeping_predict = nacho_object[["housekeeping_predict"]],
  housekeeping_norm = nacho_object[["housekeeping_norm"]],
  normalisation_method = nacho_object[["normalisation_method"]],
  n_comp = nacho_object[["n_comp"]],
  remove_outliers = nacho_object[["remove_outliers"]],
  outliers_thresholds = nacho_object[["outliers_thresholds"]]
)
```

## Arguments

- nacho_object:

  \[[list](https://rdrr.io/r/base/list.html)\] A list object of class
  `"nacho"` obtained from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or `normalise()`.

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

- remove_outliers:

  \[[logical](https://rdrr.io/r/base/logical.html)\] A boolean to
  indicate if outliers should be excluded.

- outliers_thresholds:

  \[[list](https://rdrr.io/r/base/list.html)\] List of thresholds to
  exclude outliers.

## Value

\[[list](https://rdrr.io/r/base/list.html)\] A list containing
parameters and data.

- `access`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  in `id_colname`.

- `housekeeping_genes`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or `normalise()`.

- `housekeeping_predict`:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Value passed to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md).

- `housekeeping_norm`:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Value passed to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or `normalise()`.

- `normalisation_method`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or `normalise()`.

- `remove_outliers`:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Value passed to
  `normalise()`.

- `n_comp`:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] Value passed to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md).

- `data_directory`:

  \[[character](https://rdrr.io/r/base/character.html)\] Value passed to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md).

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

  \[[list](https://rdrr.io/r/base/list.html)\] A `list` of the
  quality-control thresholds used.

- `raw_counts`:

  \[[data.frame](https://rdrr.io/r/base/data.frame.html)\] Raw counts
  with probes as rows and samples as columns. With `"CodeClass"` (first
  column), the type of the probes and `"Name"` (second column), the Name
  of the probes.

- `normalised_counts`:

  \[[data.frame](https://rdrr.io/r/base/data.frame.html)\] Normalised
  counts with probes as rows and samples as columns. With `"CodeClass"`
  (first column)), the type of the probes and `"Name"` (second column),
  the name of the probes.

## Details

Outliers definition (`remove_outliers = TRUE`):

- Binding Density (`BD`) \< 0.1

- Binding Density (`BD`) \> 2.25

- Field of View (`FoV`) \< 75

- Positive Control Linearity (`PCL`) \< 0.95

- Limit of Detection (`LoD`) \< 2

- Positive normalisation factor (`Positive_factor`) \< 0.25

- Positive normalisation factor (`Positive_factor`) \> 4

- Housekeeping normalisation factor (`house_factor`) \< 1/11

- Housekeeping normalisation factor (`house_factor`) \> 11

## Examples

``` r

data(GSE74821)
GSE74821_norm <- normalise(
  nacho_object = GSE74821,
  housekeeping_norm = TRUE,
  normalisation_method = "GEO",
  remove_outliers = TRUE
)
#> [NACHO] Normalising "GSE74821" with new value for parameters:
#>   - normalisation_method = TRUE
#>   - remove_outliers = TRUE
#> [NACHO] Computing normalisation factors using "GEO" method.
#> [NACHO] Returning a list.
#>   $ access              : character
#>   $ housekeeping_genes  : character
#>   $ housekeeping_predict: logical
#>   $ housekeeping_norm   : logical
#>   $ normalisation_method: character
#>   $ remove_outliers     : logical
#>   $ n_comp              : numeric
#>   $ data_directory      : character
#>   $ pc_sum              : data.frame
#>   $ nacho               : data.frame
#>   $ outliers_thresholds : list

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

  # (re)Normalise data by removing outliers
  nacho_norm <- normalise(
    nacho_object = nacho,
    remove_outliers = TRUE
  )

  # (re)Normalise data with "GLM" method and removing outliers
  nacho_norm <- normalise(
    nacho_object = nacho,
    normalisation_method = "GLM",
    remove_outliers = TRUE
  )
}
```
