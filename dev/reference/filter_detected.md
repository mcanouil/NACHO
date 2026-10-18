# Keep genes detected in enough samples

A gene is detected in a sample when its raw count is above the mean plus
two standard deviations of that sample's negative controls. Control and
housekeeping probes are always kept, where housekeeping probes are the
probes of CodeClass Housekeeping. Endogenous genes chosen as
housekeeping genes are filtered like any other gene, and the dropped
ones are removed from the `housekeeping_genes` setting, which becomes
`NULL` when none is left. Then the object uses its Housekeeping probes
and warns, or, with no Housekeeping probe either, turns
`housekeeping_norm` off and warns. The samples' `Detection_rate` and
`Housekeeping_detected` are not recomputed after filtering. A gene's
detection rate is the share of the samples with a detection limit in
which it is detected, so a sample with fewer than two negative probes
does not count. Endogenous genes whose detection rate is missing,
because every sample with a detection limit has a missing count for
them, are dropped, even with `min_rate = 0`. With all-zero negative
controls the detection limit is 0, so every non-zero count counts as
detected. An object without endogenous genes comes back unchanged. When
no sample has a detection limit, `filter_detected()` stops with an
error.

## Usage

``` r
filter_detected(x, min_rate = 0.5)
```

## Arguments

- x:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- min_rate:

  The smallest share of samples with a detection limit, between 0 and 1,
  in which an endogenous gene must be detected.

## Value

A `nacho` object with fewer probes.

## Examples

``` r
data(GSE74821)
dim(filter_detected(GSE74821, min_rate = 0.5))
#> [1] 72 48
```
