# Basic methods for a nacho object

Basic methods for a nacho object

## Usage

    print(x)
    format(x)
    summary(object)
    dim(x)
    as.data.frame(x, long = FALSE)
    x[i, j]

## Arguments

- `x`, `object`: A `nacho` object.

- `long`: If `TRUE`,
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
  one row per probe and sample (the NACHO 2 `$nacho` layout) instead of
  one row per sample.

- `i`: Probes to keep.

- `j`: Samples to keep.

## Examples

``` r
data(GSE74821)
print(GSE74821)
#> <nacho> 48 samples, 72 probes, from single-sample RCC files
#> Normalisation: GLM, with 8 housekeeping genes
#> Flagged samples: 0 of 48
#> Created with NACHO 3.0.0
summary(GSE74821)
#>                  metric lower upper n_fail n_missing
#> 1                    BD  0.05  2.25      0         0
#> 2                   FoV 75.00    NA      0         0
#> 3                   PCL  0.95    NA      0         0
#> 4                   LoD  2.00    NA      0         0
#> 5       Positive_factor  0.30  3.00      0         0
#> 6          House_factor  0.10 10.00      0         0
#> 7 Housekeeping_detected  3.00    NA      0         0
dim(GSE74821)
#> [1] 72 48
head(as.data.frame(GSE74821, long = TRUE)[, 1:5])
#>                                                               IDFILE
#> 1 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 2 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 3 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 4 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 5 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 6 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#>                  title geo_accession                status submission_date
#> 1 LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 2 LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 3 LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 4 LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 5 LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 6 LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
GSE74821[, 1:12]
#> <nacho> 12 samples, 72 probes, from single-sample RCC files
#> Normalisation: GLM, with 8 housekeeping genes
#> Flagged samples: 0 of 12
#> Created with NACHO 3.0.0
```
