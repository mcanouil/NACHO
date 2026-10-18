# Get the content of a nacho object

These accessors are the only public way into a `nacho` object.

## Usage

``` r
nacho_counts(x, normalised = FALSE, log2 = FALSE)

nacho_samples(x)

nacho_probes(x)

nacho_qc(x)
```

## Arguments

- x:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md).

- normalised:

  If `TRUE`, return the normalised counts instead of the raw counts.

- log2:

  If `TRUE`, return `log2(count + 1)`.

## Value

- `nacho_counts()`: a matrix with one row per probe and one column per
  sample.

- `nacho_samples()`: a data frame with one row per sample; the first
  column holds the sample ids, and the PCA scores come last (`PC01`,
  `PC02`, ...). `Detection_rate` (the share of endogenous genes
  detected) and `Housekeeping_detected` (the number of housekeeping
  genes detected) are `NA` for a sample without a detection limit, and
  `Housekeeping_detected` is also `NA` for a miRNA panel that skips
  housekeeping normalisation. miRNA panels add `Ligation_order`,
  `Ligation_R2` and `Ligation_NEG` (the ligation controls), and
  `Haemolysis`, the log2 ratio of `miR-451a` to `miR-23a-3p`, when those
  probes are present.

- `nacho_probes()`: a data frame with one row per probe: `CodeClass`,
  `Name`, `Accession`, `detection_rate` (the share of samples with a
  detection limit in which the probe is above it), `is_housekeeping` and
  `is_excluded`.

- `nacho_qc()`: a data frame with one row per sample: the id, `lane`,
  `CartridgeID`, each quality-control metric with its status (`"pass"`,
  `"fail"` or `NA` when it cannot be assessed), the other factors,
  `lane_status` for PlexSet files, `n_flags` (the sample's own failing
  metrics, not those inherited from its lane), the overall `status` and
  a readable `reason` (which also names the lane metrics a sample
  inherits).

## Examples

``` r
data(GSE74821)
nacho_counts(GSE74821)[1:5, 1:3]
#>        GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> ACTR3B                                                                772
#> ANLN                                                                 1762
#> BAG1                                                                 2535
#> BCL2                                                                  383
#> BIRC5                                                                 682
#>        GSM1934698_20111102_20111102-9741-1_LGWU102547-1036381WU_05.RCC.gz
#> ACTR3B                                                                459
#> ANLN                                                                 1250
#> BAG1                                                                 1451
#> BCL2                                                                  122
#> BIRC5                                                                2516
#>        GSM1934699_20111102_20111102-9741-1_LGWU102569-1036382WU_06.RCC.gz
#> ACTR3B                                                                431
#> ANLN                                                                 2672
#> BAG1                                                                  877
#> BCL2                                                                  400
#> BIRC5                                                                1386
head(nacho_samples(GSE74821)[, 1:5])
#>                                                               IDFILE
#> 1 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz
#> 2 GSM1934698_20111102_20111102-9741-1_LGWU102547-1036381WU_05.RCC.gz
#> 3 GSM1934699_20111102_20111102-9741-1_LGWU102569-1036382WU_06.RCC.gz
#> 4 GSM1934700_20111102_20111102-9741-1_LGWU102614-1036385WU_09.RCC.gz
#> 5 GSM1934701_20111102_20111102-9741-1_LGWU102680-1036384WU_08.RCC.gz
#> 6 GSM1934702_20111102_20111102-9741-1_LGWU102731-1036388WU_12.RCC.gz
#>                  title geo_accession                status submission_date
#> 1 LGWU101986-1036378WU    GSM1934697 Public on Nov 10 2015     Nov 09 2015
#> 2 LGWU102547-1036381WU    GSM1934698 Public on Nov 10 2015     Nov 09 2015
#> 3 LGWU102569-1036382WU    GSM1934699 Public on Nov 10 2015     Nov 09 2015
#> 4 LGWU102614-1036385WU    GSM1934700 Public on Nov 10 2015     Nov 09 2015
#> 5 LGWU102680-1036384WU    GSM1934701 Public on Nov 10 2015     Nov 09 2015
#> 6 LGWU102731-1036388WU    GSM1934702 Public on Nov 10 2015     Nov 09 2015
head(nacho_probes(GSE74821))
#>    CodeClass   Name      Accession detection_rate is_housekeeping is_excluded
#> 1 Endogenous ACTR3B NM_001040135.1              1           FALSE       FALSE
#> 2 Endogenous   ANLN    NM_018685.2              1           FALSE       FALSE
#> 3 Endogenous   BAG1    NM_004323.3              1           FALSE       FALSE
#> 4 Endogenous   BCL2    NM_000633.2              1           FALSE       FALSE
#> 5 Endogenous  BIRC5    NM_001168.2              1           FALSE       FALSE
#> 6 Endogenous  BLVRA    NM_000712.3              1           FALSE       FALSE
head(nacho_qc(GSE74821))
#>                                                               IDFILE lane
#> 1 GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz    2
#> 2 GSM1934698_20111102_20111102-9741-1_LGWU102547-1036381WU_05.RCC.gz    5
#> 3 GSM1934699_20111102_20111102-9741-1_LGWU102569-1036382WU_06.RCC.gz    6
#> 4 GSM1934700_20111102_20111102-9741-1_LGWU102614-1036385WU_09.RCC.gz    9
#> 5 GSM1934701_20111102_20111102-9741-1_LGWU102680-1036384WU_08.RCC.gz    8
#> 6 GSM1934702_20111102_20111102-9741-1_LGWU102731-1036388WU_12.RCC.gz   12
#>       CartridgeID   BD BD_status    FoV FoV_status     PCL PCL_status   LoD
#> 1 20111102-9741-1 0.30      pass 100.00       pass 0.99938       pass 15.46
#> 2 20111102-9741-1 0.19      pass 100.00       pass 0.99915       pass 24.57
#> 3 20111102-9741-1 0.17      pass  93.94       pass 0.99895       pass 17.39
#> 4 20111102-9741-1 0.15      pass  99.91       pass 0.99979       pass 39.36
#> 5 20111102-9741-1 0.25      pass 100.00       pass 0.99875       pass 23.72
#> 6 20111102-9741-1 0.20      pass 100.00       pass 0.99950       pass 22.34
#>   LoD_status Positive_factor Positive_factor_status House_factor
#> 1       pass       0.8076212                   pass    0.6237488
#> 2       pass       0.8483066                   pass    0.8268550
#> 3       pass       0.8974077                   pass    1.0295163
#> 4       pass       0.8044223                   pass    1.7929444
#> 5       pass       0.8686345                   pass    1.1841057
#> 6       pass       0.8500153                   pass    1.2040930
#>   House_factor_status Housekeeping_detected Housekeeping_detected_status
#> 1                pass                     8                         pass
#> 2                pass                     8                         pass
#> 3                pass                     8                         pass
#> 4                pass                     8                         pass
#> 5                pass                     8                         pass
#> 6                pass                     8                         pass
#>        MC  MedC Negative_factor Background n_flags status reason
#> 1 2049.32 672.0       16.663102         NA       0   pass   <NA>
#> 2 1567.46 988.0        9.604329         NA       0   pass   <NA>
#> 3  918.48 611.0        9.716379         NA       0   pass   <NA>
#> 4  758.02 333.0       10.457253         NA       0   pass   <NA>
#> 5 2711.44 366.0       15.492908         NA       0   pass   <NA>
#> 6 1493.18 459.5       13.464436         NA       0   pass   <NA>
```
