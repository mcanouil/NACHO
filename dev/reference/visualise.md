# Explore and tune quality control in the NACHO app

Opens
[`nacho_app()`](https://m.canouil.dev/NACHO/dev/reference/nacho_app.md)
on `nacho_object`. Change thresholds and normalisation settings in the
sidebar, then click "Done" to close the app and get the tuned object
back.

## Usage

``` r
visualise(nacho_object)
```

## Arguments

- nacho_object:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

## Value

The tuned `nacho` object, invisibly, once you click "Done".

## Examples

``` r
if (interactive()) {
  data(GSE74821)
  tuned <- visualise(GSE74821)
  nacho_qc(tuned)
}

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
  visualise(nacho)

  # Drop the outliers and normalise the other samples again
  visualise(exclude_outliers(nacho))

  # Normalise with the "GLM" method, then drop the outliers
  nacho_glm <- normalise(nacho, normalisation_method = "GLM")
  visualise(exclude_outliers(nacho_glm))
}
```
