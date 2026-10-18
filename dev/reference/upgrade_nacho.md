# Convert a NACHO 2 object to NACHO 3

NACHO 3 stores data in an S7 `nacho` object instead of the NACHO 2 list.
`upgrade_nacho()` converts a NACHO 2 object once: it keeps the raw
counts, the sample sheet, the settings and the thresholds, and computes
the quality-control metrics, the normalised counts, the PCA and the
outlier flags again with the NACHO 3 definitions.

## Usage

``` r
upgrade_nacho(x)
```

## Arguments

- x:

  A NACHO 2 object, as returned by
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  in NACHO 2.

## Value

A `nacho` object.

## See also

[`read_nacho()`](https://m.canouil.dev/NACHO/dev/reference/read_nacho.md)

## Examples

``` r
# A NACHO 2 object is converted; GSE74821 is already a NACHO 3 object,
# so it comes back unchanged.
data(GSE74821)
identical(upgrade_nacho(GSE74821), GSE74821)
#> `x` is already a NACHO 3 object, so it is returned unchanged.
#> [1] TRUE
```
