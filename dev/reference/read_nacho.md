# Read a saved NACHO object

Reads an `.rds` file written with
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html) from any NACHO
version. NACHO 2 objects go through
[`upgrade_nacho()`](https://m.canouil.dev/NACHO/dev/reference/upgrade_nacho.md).
NACHO 3 objects are rebuilt with the current class, because each saved
S7 object carries a copy of the class it was made with.

## Usage

``` r
read_nacho(path)
```

## Arguments

- path:

  Path to an `.rds` file.

## Value

A `nacho` object.

## Details

Only read files you trust:
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) can run code stored
in a crafted file, notably in R before 4.4.0.

## See also

[`upgrade_nacho()`](https://m.canouil.dev/NACHO/dev/reference/upgrade_nacho.md)

## Examples

``` r
path <- tempfile(fileext = ".rds")
saveRDS(GSE74821, path)
read_nacho(path)
#> <nacho> 48 samples, 72 probes, from single-sample RCC files
#> Normalisation: GLM, with 8 housekeeping genes
#> Flagged samples: 0 of 48
#> Created with NACHO 3.0.0
```
