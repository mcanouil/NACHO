# Drop outliers and normalise the other samples again

Removes the samples whose `status` in
[`nacho_qc()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md)
is `"fail"` and runs the normalisation again on the samples that are
left, with the same settings and thresholds. The new factors can flag
more samples; call `exclude_outliers()` again to drop those too.

## Usage

``` r
exclude_outliers(nacho_object)
```

## Arguments

- nacho_object:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

## Value

A `nacho` object without the flagged samples.

## Examples

``` r
data(GSE74821)
ncol(exclude_outliers(GSE74821))
#> No sample is flagged, so nothing is removed.
#> [1] 48
```
