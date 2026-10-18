# Convert between nacho objects and Bioconductor containers

`as_summarized_experiment()` gives a `SummarizedExperiment` with the raw
and normalised counts as assays `counts` and `normalised`,
[`nacho_samples()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md)
as `colData`,
[`nacho_probes()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md)
as `rowData`, and the settings, thresholds, RCC type and provenance in
`metadata()$nacho`.

## Usage

``` r
as_summarized_experiment(x)

as_nacho(x, id_colname = "IDFILE")
```

## Arguments

- x:

  A `nacho` object for `as_summarized_experiment()`; a
  `SummarizedExperiment` or a `NanoStringRccSet` for `as_nacho()`.

- id_colname:

  Name of the sample id column to create when `x` does not come from
  NACHO. When `metadata(x)$nacho$settings` exists, its `id_colname` wins
  and this argument is ignored.

## Value

A `SummarizedExperiment`, or a `nacho` object.

## Details

`as_nacho()` goes the other way, from a `SummarizedExperiment` or from a
`NanoStringRccSet` read by
[`NanoStringNCTools::readNanoStringRccSet()`](https://rdrr.io/pkg/NanoStringNCTools/man/readNanoStringRccSet.html).
The dates of a `NanoStringRccSet` are written as `YYYYMMDD`, as in RCC
files. Its probe names lose a trailing numeric suffix such as `|0`, as
in
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md),
so both give the same probe names for the same RCC files. It computes
the quality-control metrics and the normalisation from the raw counts;
settings and thresholds come from `metadata()$nacho` when present, and
are the defaults otherwise. They are checked like the arguments of
[`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).
The result always carries the current object schema. A `nacho` object is
checked and returned as is, and a NACHO 2 object gets a hint to use
[`upgrade_nacho()`](https://m.canouil.dev/NACHO/dev/reference/upgrade_nacho.md).

## Examples

``` r
if (requireNamespace("SummarizedExperiment", quietly = TRUE)) {
  se <- as_summarized_experiment(GSE74821)
  as_nacho(se)
}
#> <nacho> 48 samples, 72 probes, from single-sample RCC files
#> Normalisation: GLM, with 8 housekeeping genes
#> Flagged samples: 0 of 48
#> Created with NACHO 2.0.7.9000
```
