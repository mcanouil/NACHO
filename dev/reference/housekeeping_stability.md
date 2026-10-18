# Rank genes by expression stability

Ranks candidate reference genes with geNorm (Vandesompele et al. 2002)
and NormFinder (Andersen et al. 2004), after keeping the genes detected
in most samples. With `group`, NormFinder accounts for the groups, and a
Kruskal-Wallis test tells whether each gene's expression differs between
them, which makes a poor reference gene.

## Usage

``` r
housekeeping_stability(x, genes = NULL, group = NULL, min_detection = 0.9)
```

## Arguments

- x:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- genes:

  The genes to rank; `NULL` ranks the `Housekeeping` probes.

- group:

  A column of `nacho_samples(x)` with the biological groups, or `NULL`.

- min_detection:

  The smallest share of samples in which a gene must be above background
  to be ranked.

## Value

A list with `ranking`, a data frame with one row per gene from the most
to the least stable: `Name`, `CodeClass`, `detection_rate`, `mean_log2`,
`geNorm_M`, `geNorm_rank`, `NormFinder_rho` and, with `group`,
`group_p_value` and `group_p_adjusted` (Benjamini-Hochberg); and
`pairwise_v`, the geNorm pairwise variation `V` for each number of
genes. A `V` below 0.15 is the usual sign that adding a gene no longer
helps.

## Details

The two most stable genes share the same geNorm M by construction, so
their order, ranks 1 and 2, is arbitrary. Without `group`,
`NormFinder_rho` is the standard deviation of the residual variation of
the gene. With `group`, it also counts the variation between the groups,
shrunk towards zero as NormFinder does.

## Examples

``` r
data(GSE74821)
housekeeping_stability(GSE74821)$ranking
#>     Name    CodeClass detection_rate mean_log2  geNorm_M geNorm_rank
#> 1   PUM1 Housekeeping              1  9.706594 0.5939004           1
#> 2  SF3A1 Housekeeping              1 10.061556 0.5538149           2
#> 3 MRPL19 Housekeeping              1  9.656192 0.5740756           3
#> 4  PSMC4 Housekeeping              1 10.048324 0.6391309           4
#> 5   ACTB Housekeeping              1 17.201411 0.6534672           5
#> 6  RPLP0 Housekeeping              1 14.826392 0.7015339           6
#> 7   GUSB Housekeeping              1  9.701745 0.7443183           7
#> 8   TFRC Housekeeping              1  9.873900 0.8469462           8
#>   NormFinder_rho
#> 1      0.3618938
#> 2      0.2564156
#> 3      0.2979553
#> 4      0.4318650
#> 5      0.4470580
#> 6      0.5310189
#> 7      0.5975082
#> 8      0.7322992
```
