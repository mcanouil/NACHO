# Suggest how many unwanted factors RUVg should remove

For each `k` from 0 to `max_k`, corrects the log2 positive-normalised
counts with RUVg, using the housekeeping genes as controls, and reports
the mean interquartile range of the relative log expression (RLE) and
the share of variance on the first principal component. The suggested
`k` is the smallest whose RLE spread is within 5 % of the smallest one:
removing more factors risks removing biology.

## Usage

``` r
suggest_ruv_k(x, max_k = 5)
```

## Arguments

- x:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- max_k:

  The largest `k` to try. It is lowered to one less than the number of
  housekeeping genes, and to two less than the number of samples, when
  those are smaller. A `k` above that cap fits the samples exactly and
  flattens the RLE spread, so it is never tried. With one or two samples
  only `k = 0` is tried.

## Value

A data frame with `k`, `rle_iqr`, `pc1_variance` and `suggested`.
`pc1_variance` is `NA` when the corrected values have no variance, as
with one sample.

## Examples

``` r
data(GSE74821)
suggest_ruv_k(GSE74821)
#>   k  rle_iqr pc1_variance suggested
#> 1 0 1.333779    0.3216721     FALSE
#> 2 1 1.287619    0.3542053     FALSE
#> 3 2 1.215446    0.3354141     FALSE
#> 4 3 1.195121    0.3528246     FALSE
#> 5 4 1.154325    0.3223319      TRUE
#> 6 5 1.121317    0.3358693     FALSE
```
