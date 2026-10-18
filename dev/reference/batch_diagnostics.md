# Batch and confounding diagnostics

Checks whether the study design, the quality-control metrics and the
main axes of variation follow technical batches such as cartridges and
run dates.

## Usage

``` r
batch_diagnostics(x, group = NULL, batch = c("CartridgeID", "Date"))
```

## Arguments

- x:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- group:

  A column of `nacho_samples(x)` with the biological groups, or `NULL`
  to skip the design check.

- batch:

  Columns of `nacho_samples(x)` that describe technical batches.

## Value

A list:

- `design`: `NULL` without `group`; otherwise, for each batch variable,
  its number of levels, Cramér's V with `group`, the number of levels
  that hold a single group, and `confounded`, `TRUE` when at least one
  does. A level counts as holding a single group only when it has two or
  more samples with a group, so a cartridge with one sample never makes
  the design confounded. Cramér's V uses only the samples that have both
  a group and a batch, and is `NA` when the batch or `group` has fewer
  than two levels among them.

- `crosstabs`: `NULL` without `group`; otherwise the table of `group`
  against each batch variable.

- `metrics`: Kruskal-Wallis tests of each quality-control metric against
  each batch variable.

- `pc_batch`: the share of each principal component's variance explained
  by each batch variable.

`metrics` and `pc_batch` are data frames with these columns, and have no
rows when there is nothing to report. A batch variable with one level
gives `NA` in `statistic`, `p_value` and `r_squared`, and a constant
metric gives `NA` in `statistic` and `p_value`. A batch variable whose
levels each hold one sample with a value also gives `NA` in `r_squared`.
For PlexSet data, `BD` and `FoV` are tested once per lane.

## Details

Start with `design`: when a batch level holds a single group, batch and
biology cannot be told apart, and no normalisation can fix it.

## Examples

``` r
data(GSE74821)
batch_diagnostics(GSE74821)$pc_batch
#>      PC       batch    r_squared
#> 1  PC01 CartridgeID 8.669160e-03
#> 2  PC02 CartridgeID 1.812515e-01
#> 3  PC03 CartridgeID 1.482027e-02
#> 4  PC04 CartridgeID 1.697497e-01
#> 5  PC05 CartridgeID 6.387001e-02
#> 6  PC06 CartridgeID 4.405153e-02
#> 7  PC07 CartridgeID 5.334040e-02
#> 8  PC08 CartridgeID 3.552575e-02
#> 9  PC09 CartridgeID 1.007797e-01
#> 10 PC10 CartridgeID 1.599434e-01
#> 11 PC01        Date 4.242826e-03
#> 12 PC02        Date 4.119176e-02
#> 13 PC03        Date 1.343894e-03
#> 14 PC04        Date 7.225959e-02
#> 15 PC05        Date 1.169531e-02
#> 16 PC06        Date 2.831487e-02
#> 17 PC07        Date 5.279313e-02
#> 18 PC08        Date 6.286744e-05
#> 19 PC09        Date 2.957369e-02
#> 20 PC10        Date 1.090615e-02
```
