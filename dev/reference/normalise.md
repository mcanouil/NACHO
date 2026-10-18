# (Re)normalise a nacho object

(Re)normalise a nacho object

## Usage

``` r
normalise(
  nacho_object,
  housekeeping_genes = nacho_object@settings[["housekeeping_genes"]],
  housekeeping_predict = nacho_object@settings[["housekeeping_predict"]],
  housekeeping_norm = nacho_object@settings[["housekeeping_norm"]],
  normalisation_method = nacho_object@settings[["normalisation_method"]],
  background = nacho_object@settings[["background"]],
  background_mode = nacho_object@settings[["background_mode"]],
  ruv_k = nacho_object@settings[["ruv_k"]],
  n_comp = nacho_object@settings[["n_comp"]],
  outliers_thresholds = nacho_object@thresholds,
  ...
)
```

## Arguments

- nacho_object:

  A `nacho` object from
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or `normalise()`.

- housekeeping_genes:

  \[[character](https://rdrr.io/r/base/character.html)\] A vector of
  names of the miRNAs/mRNAs that should be used as housekeeping genes.
  Default is `NULL`.

- housekeeping_predict:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the housekeeping genes should be predicted (`TRUE`) or not
  (`FALSE`). Default is `FALSE`. Prediction picks the five most stable
  genes by geNorm on the raw counts. It is skipped for the miRNA methods
  of `normalisation_method`.

- housekeeping_norm:

  \[[logical](https://rdrr.io/r/base/logical.html)\] Boolean to indicate
  whether the housekeeping normalisation should be performed. The
  default is the setting stored in `nacho_object`, which is `TRUE` or
  `FALSE`.

- normalisation_method:

  \[[character](https://rdrr.io/r/base/character.html)\] `"GEO"` (the
  default) or `"GLM"` scale samples by their positive controls, with the
  geometric mean or a Poisson model, then by the housekeeping genes;
  `"RUVg"` scales by the positive controls with the geometric mean, then
  removes `ruv_k` factors of unwanted variation estimated from the
  housekeeping genes, which
  [`nacho_samples()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md)
  returns as `W_1`, `W_2`, ... for use as covariates. For miRNA panels,
  `"stable_mirna"`, `"total_mirna"`, `"spike_in"` and `"ligation"` scale
  by the five most stable miRNAs, the miRNAs above 50 counts, the
  spike-in controls or the ligation positive controls, in the order
  Bruker recommends for plasma and serum. These methods ignore
  `housekeeping_genes`, `housekeeping_predict` and `housekeeping_norm`,
  and give their scaling factor as `House_factor`. When a GLM fit fails,
  NACHO warns and uses the geometric mean instead, as `normalise()`
  describes.

- background:

  \[[character](https://rdrr.io/r/base/character.html)\] How to estimate
  each sample's background from its negative controls: `"none"` (the
  default), `"mean"`, `"mean_2sd"` (mean plus two standard deviations),
  `"median"`, `"max"` or `"geo"` (geometric mean).

- background_mode:

  \[[character](https://rdrr.io/r/base/character.html)\] What to do with
  that background: `"threshold"` (the default) raises lower counts to
  it, which Bruker recommends when fold changes matter, and `"subtract"`
  removes it and floors at 0.

- ruv_k:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] The number of
  unwanted factors RUVg removes. `normalise()` reuses the `ruv_k` stored
  in the object, so pass `ruv_k = NULL` to have
  [`suggest_ruv_k()`](https://m.canouil.dev/NACHO/dev/reference/suggest_ruv_k.md)
  choose again. A `ruv_k` above two less than the number of samples, or
  one less than the number of control genes, is lowered with a
  `nacho_warning_ruv_k_reduced` warning. Other methods ignore it.

- n_comp:

  \[[numeric](https://rdrr.io/r/base/numeric.html)\] Number indicating
  the number of principal components to compute. Cannot be more than n-1
  samples. Default is `10`.

- outliers_thresholds:

  A list of quality-control thresholds, as returned by
  [`nacho_thresholds()`](https://m.canouil.dev/NACHO/dev/reference/nacho_thresholds.md).

- ...:

  Must be empty.

## Value

A `nacho` object.

## Details

When only the limits in `outliers_thresholds` change and the preset
stays the same, `normalise()` keeps the counts and stores the new
thresholds; the flags follow them whenever you read
[`nacho_qc()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md).
Otherwise it computes the quality-control metrics, the normalisation
factors, the normalised counts and the PCA again.

[`nacho_qc()`](https://m.canouil.dev/NACHO/dev/reference/nacho-accessors.md)
lists the flags, and
[`exclude_outliers()`](https://m.canouil.dev/NACHO/dev/reference/exclude_outliers.md)
drops the flagged samples.

With `normalisation_method = "GLM"`, each sample's positive and negative
control counts plus 1 are fitted against their known concentrations with
a Poisson GLM and an identity link, and the positive factor is the mean
slope over the sample's slope. When the fit fails for any sample, NACHO
warns and uses the geometric mean for every sample, so all samples stay
on the same scale. The recorded `normalisation_method` then becomes
`"GEO"`, and the failed samples are listed in `provenance$glm_fallback`.

The normalisation runs in this order: raw counts, negative probe
exclusion, background (`background` and `background_mode`), positive
control factor (`normalisation_method`), then content factor
(housekeeping genes when `housekeeping_norm` is `TRUE`). RUVg works on
`log2(count + 1)` after the positive factor and returns counts on the
count scale, floored at 0. It replaces the housekeeping scaling, so
`housekeeping_norm` has no effect and `House_factor` is not computed,
and it corrects only the endogenous and housekeeping probes. The miRNA
methods `"stable_mirna"`, `"total_mirna"`, `"spike_in"` and `"ligation"`
also ignore `housekeeping_norm`, `housekeeping_genes` and
`housekeeping_predict`. They store their factor in `House_factor`, which
is judged against the `House_factor` threshold, and the probes they used
in `provenance$content_probes`. Normalised counts are otherwise neither
rounded nor floored; use
`nacho_counts(x, normalised = TRUE, log2 = TRUE)` for `log2(count + 1)`.

## Examples

``` r
data(GSE74821)
GSE74821_geo <- normalise(GSE74821, normalisation_method = "GEO")
#> Normalising again with new settings:
#> • normalisation_method
nacho_qc(GSE74821_geo)
#>                                                                     IDFILE lane
#> 1       GSM1934697_20111102_20111102-9741-1_LGWU101986-1036378WU_02.RCC.gz    2
#> 2       GSM1934698_20111102_20111102-9741-1_LGWU102547-1036381WU_05.RCC.gz    5
#> 3       GSM1934699_20111102_20111102-9741-1_LGWU102569-1036382WU_06.RCC.gz    6
#> 4       GSM1934700_20111102_20111102-9741-1_LGWU102614-1036385WU_09.RCC.gz    9
#> 5       GSM1934701_20111102_20111102-9741-1_LGWU102680-1036384WU_08.RCC.gz    8
#> 6       GSM1934702_20111102_20111102-9741-1_LGWU102731-1036388WU_12.RCC.gz   12
#> 7       GSM1934703_20111102_20111102-9741-1_LGWU107295-1036383WU_07.RCC.gz    7
#> 8       GSM1934704_20111102_20111102-9741-1_LGWU201357-1036377WU_01.RCC.gz    1
#> 9       GSM1934705_20111102_20111102-9741-1_LGWU702410-1036379WU_03.RCC.gz    3
#> 10      GSM1934706_20111102_20111102-9741-1_LGWU702420-1036380WU_04.RCC.gz    4
#> 11      GSM1934707_20111102_20111102-9741-1_LGWU702568-1036386WU_10.RCC.gz   10
#> 12      GSM1934708_20111102_20111102-9741-1_LGWU702589-1036387WU_11.RCC.gz   11
#> 13      GSM1934709_20111102_20111102-9741-2_LGWU102776-1036390WU_02.RCC.gz    2
#> 14      GSM1934710_20111102_20111102-9741-2_LGWU109257-1036400WU_12.RCC.gz   12
#> 15      GSM1934711_20111102_20111102-9741-2_LGWU109355-1036389WU_01.RCC.gz    1
#> 16      GSM1934712_20111102_20111102-9741-2_LGWU702272-1036391WU_03.RCC.gz    3
#> 17      GSM1934713_20111102_20111102-9741-2_LGWU702707-1036392WU_04.RCC.gz    4
#> 18      GSM1934714_20111102_20111102-9741-2_LGWU702741-1036395WU_07.RCC.gz    7
#> 19      GSM1934715_20111102_20111102-9741-2_LGWU703193-1036393WU_05.RCC.gz    5
#> 20      GSM1934716_20111102_20111102-9741-2_LGWU703194-1036394WU_06.RCC.gz    6
#> 21      GSM1934717_20111102_20111102-9741-2_LGWU703229-1036396WU_08.RCC.gz    8
#> 22      GSM1934718_20111102_20111102-9741-2_LGWU703282-1036397WU_09.RCC.gz    9
#> 23      GSM1934719_20111102_20111102-9741-2_LGWU703286-1036398WU_10.RCC.gz   10
#> 24      GSM1934720_20111102_20111102-9741-2_LGWU703330-1036399WU_11.RCC.gz   11
#> 25 GSM1935695_20111230_20111228-9741-2-0044_LGWU102471-1037527WU_04.RCC.gz    4
#> 26 GSM1935696_20111230_20111228-9741-2-0044_LGWU200389-1037525WU_02.RCC.gz    2
#> 27 GSM1935697_20111230_20111228-9741-2-0044_LGWU201668-1037526WU_03.RCC.gz    3
#> 28 GSM1935698_20111230_20111228-9741-2-0044_LGWU203345-1037530WU_07.RCC.gz    7
#> 29 GSM1935699_20111230_20111228-9741-2-0044_LGWU205703-1037528WU_05.RCC.gz    5
#> 30 GSM1935700_20111230_20111228-9741-2-0044_LGWU220412-1037524WU_01.RCC.gz    1
#> 31 GSM1935701_20111230_20111228-9741-2-0044_LGWU702289-1037533WU_10.RCC.gz   10
#> 32 GSM1935702_20111230_20111228-9741-2-0044_LGWU702398-1037529WU_06.RCC.gz    6
#> 33 GSM1935703_20111230_20111228-9741-2-0044_LGWU703117-1037531WU_08.RCC.gz    8
#> 34 GSM1935704_20111230_20111228-9741-2-0044_LGWU703148-1037532WU_09.RCC.gz    9
#> 35 GSM1935705_20111230_20111228-9741-2-0044_LGWU703212-1037534WU_11.RCC.gz   11
#> 36 GSM1935706_20111230_20111228-9741-2-0044_LGWU703244-1037535WU_12.RCC.gz   12
#> 37 GSM1935718_20111230_20111228-9741-4-0044_LGWU102603-1037556WU_09.RCC.gz    9
#> 38 GSM1935719_20111230_20111228-9741-4-0044_LGWU102672-1037558WU_11.RCC.gz   11
#> 39 GSM1935720_20111230_20111228-9741-4-0044_LGWU102709-1037559WU_12.RCC.gz   12
#> 40 GSM1935721_20111230_20111228-9741-4-0044_LGWU107327-1037555WU_08.RCC.gz    8
#> 41 GSM1935722_20111230_20111228-9741-4-0044_LGWU109085-1037557WU_10.RCC.gz   10
#> 42 GSM1935723_20111230_20111228-9741-4-0044_LGWU702205-1037549WU_02.RCC.gz    2
#> 43 GSM1935724_20111230_20111228-9741-4-0044_LGWU702206-1037550WU_03.RCC.gz    3
#> 44 GSM1935725_20111230_20111228-9741-4-0044_LGWU702209-1037552WU_05.RCC.gz    5
#> 45 GSM1935726_20111230_20111228-9741-4-0044_LGWU702211-1037553WU_06.RCC.gz    6
#> 46 GSM1935727_20111230_20111228-9741-4-0044_LGWU702494-1037548WU_01.RCC.gz    1
#> 47 GSM1935728_20111230_20111228-9741-4-0044_LGWU702510-1037551WU_04.RCC.gz    4
#> 48 GSM1935729_20111230_20111228-9741-4-0044_LGWU702533-1037554WU_07.RCC.gz    7
#>             CartridgeID   BD BD_status    FoV FoV_status     PCL PCL_status
#> 1       20111102-9741-1 0.30      pass 100.00       pass 0.99938       pass
#> 2       20111102-9741-1 0.19      pass 100.00       pass 0.99915       pass
#> 3       20111102-9741-1 0.17      pass  93.94       pass 0.99895       pass
#> 4       20111102-9741-1 0.15      pass  99.91       pass 0.99979       pass
#> 5       20111102-9741-1 0.25      pass 100.00       pass 0.99875       pass
#> 6       20111102-9741-1 0.20      pass 100.00       pass 0.99950       pass
#> 7       20111102-9741-1 0.13      pass 100.00       pass 0.99964       pass
#> 8       20111102-9741-1 0.16      pass  99.83       pass 0.99953       pass
#> 9       20111102-9741-1 0.24      pass  99.91       pass 0.99972       pass
#> 10      20111102-9741-1 0.22      pass  99.91       pass 0.99886       pass
#> 11      20111102-9741-1 0.20      pass  99.91       pass 0.99979       pass
#> 12      20111102-9741-1 0.19      pass 100.00       pass 0.99991       pass
#> 13      20111102-9741-2 0.12      pass  99.91       pass 0.99975       pass
#> 14      20111102-9741-2 0.13      pass  99.83       pass 0.99926       pass
#> 15      20111102-9741-2 0.13      pass  99.91       pass 0.99925       pass
#> 16      20111102-9741-2 0.15      pass 100.00       pass 0.99945       pass
#> 17      20111102-9741-2 0.14      pass 100.00       pass 0.99985       pass
#> 18      20111102-9741-2 0.21      pass  99.74       pass 0.99882       pass
#> 19      20111102-9741-2 0.16      pass 100.00       pass 0.99937       pass
#> 20      20111102-9741-2 0.14      pass  99.83       pass 0.99921       pass
#> 21      20111102-9741-2 0.16      pass 100.00       pass 0.99958       pass
#> 22      20111102-9741-2 0.21      pass  99.57       pass 0.99964       pass
#> 23      20111102-9741-2 0.14      pass  99.91       pass 0.99936       pass
#> 24      20111102-9741-2 0.19      pass  99.83       pass 0.99977       pass
#> 25 20111228-9741-2-0044 0.11      pass  99.91       pass 0.99925       pass
#> 26 20111228-9741-2-0044 0.22      pass  99.65       pass 0.99898       pass
#> 27 20111228-9741-2-0044 0.13      pass  99.91       pass 0.99806       pass
#> 28 20111228-9741-2-0044 0.25      pass  99.74       pass 0.99895       pass
#> 29 20111228-9741-2-0044 0.19      pass  99.83       pass 0.99824       pass
#> 30 20111228-9741-2-0044 0.22      pass  99.13       pass 0.99921       pass
#> 31 20111228-9741-2-0044 0.16      pass  99.74       pass 0.99899       pass
#> 32 20111228-9741-2-0044 0.27      pass  99.74       pass 0.99916       pass
#> 33 20111228-9741-2-0044 0.18      pass 100.00       pass 0.99901       pass
#> 34 20111228-9741-2-0044 0.15      pass 100.00       pass 0.99950       pass
#> 35 20111228-9741-2-0044 0.20      pass 100.00       pass 0.99898       pass
#> 36 20111228-9741-2-0044 0.18      pass  99.48       pass 0.99794       pass
#> 37 20111228-9741-4-0044 0.20      pass  99.83       pass 0.99799       pass
#> 38 20111228-9741-4-0044 0.18      pass  99.65       pass 0.99786       pass
#> 39 20111228-9741-4-0044 0.14      pass  99.83       pass 0.99803       pass
#> 40 20111228-9741-4-0044 0.16      pass  99.83       pass 0.99896       pass
#> 41 20111228-9741-4-0044 0.21      pass  99.74       pass 0.99807       pass
#> 42 20111228-9741-4-0044 0.30      pass 100.00       pass 0.99806       pass
#> 43 20111228-9741-4-0044 0.24      pass  99.91       pass 0.99832       pass
#> 44 20111228-9741-4-0044 0.22      pass 100.00       pass 0.99799       pass
#> 45 20111228-9741-4-0044 0.21      pass  99.74       pass 0.99910       pass
#> 46 20111228-9741-4-0044 0.24      pass  99.91       pass 0.99777       pass
#> 47 20111228-9741-4-0044 0.23      pass  99.83       pass 0.99907       pass
#> 48 20111228-9741-4-0044 0.26      pass 100.00       pass 0.99809       pass
#>      LoD LoD_status Positive_factor Positive_factor_status House_factor
#> 1  15.46       pass       0.7926544                   pass    0.6372416
#> 2  24.57       pass       0.8205999                   pass    0.8570799
#> 3  17.39       pass       0.8944521                   pass    1.0357059
#> 4  39.36       pass       0.7916595                   pass    1.8267664
#> 5  23.72       pass       0.8239102                   pass    1.2517518
#> 6  22.34       pass       0.8465109                   pass    1.2123410
#> 7  18.30       pass       0.9035223                   pass    2.8773480
#> 8  16.69       pass       1.2729639                   pass    0.8969282
#> 9  12.42       pass       0.8498383                   pass    0.9588289
#> 10 26.51       pass       0.8108045                   pass    1.1959501
#> 11 27.94       pass       0.8223599                   pass    0.9850056
#> 12 19.78       pass       0.7027720                   pass    1.3050989
#> 13 21.70       pass       0.8358269                   pass    2.5625234
#> 14 21.60       pass       1.3730049                   pass    1.2603067
#> 15 19.26       pass       1.2568087                   pass    1.0408657
#> 16 29.77       pass       0.8230503                   pass    2.0984907
#> 17 28.12       pass       0.8571008                   pass    1.6045494
#> 18 16.34       pass       0.9586144                   pass    0.7671594
#> 19 19.52       pass       0.8386164                   pass    1.3911701
#> 20 26.50       pass       0.8936039                   pass    1.6492852
#> 21 22.15       pass       0.8250703                   pass    1.5498742
#> 22 27.34       pass       0.8870244                   pass    1.2151086
#> 23 23.84       pass       1.0573637                   pass    1.6153952
#> 24 18.36       pass       0.9821169                   pass    1.1054763
#> 25 36.02       pass       0.9736706                   pass    3.3386079
#> 26 18.69       pass       1.1555223                   pass    0.9456928
#> 27 20.39       pass       1.3731937                   pass    1.9562274
#> 28 17.89       pass       1.1475746                   pass    0.6390146
#> 29 19.68       pass       0.9877873                   pass    0.7735861
#> 30 24.17       pass       1.0932201                   pass    1.0355452
#> 31 35.95       pass       1.2864852                   pass    1.0531155
#> 32 14.10       pass       1.3553496                   pass    0.6515782
#> 33 18.45       pass       1.1921277                   pass    0.9607932
#> 34 16.30       pass       1.0870175                   pass    1.2429479
#> 35 13.78       pass       1.1289401                   pass    0.7157251
#> 36 10.94       pass       1.6583405                   pass    0.6046393
#> 37 15.28       pass       1.1796725                   pass    1.0089195
#> 38 18.70       pass       1.3843009                   pass    0.7357616
#> 39 27.92       pass       1.1213794                   pass    1.5692629
#> 40 30.61       pass       1.0319040                   pass    1.0993663
#> 41 16.52       pass       1.2186233                   pass    0.7426318
#> 42 14.50       pass       1.0014305                   pass    0.7242309
#> 43 26.64       pass       1.1064350                   pass    0.8415001
#> 44 13.68       pass       1.2078480                   pass    0.7707609
#> 45 13.00       pass       1.2726418                   pass    0.7572489
#> 46 21.58       pass       0.9950025                   pass    0.7452620
#> 47 49.61       pass       0.9116339                   pass    0.8786638
#> 48 22.58       pass       1.0094136                   pass    0.4636502
#>    House_factor_status Housekeeping_detected Housekeeping_detected_status
#> 1                 pass                     8                         pass
#> 2                 pass                     8                         pass
#> 3                 pass                     8                         pass
#> 4                 pass                     8                         pass
#> 5                 pass                     8                         pass
#> 6                 pass                     8                         pass
#> 7                 pass                     8                         pass
#> 8                 pass                     8                         pass
#> 9                 pass                     8                         pass
#> 10                pass                     8                         pass
#> 11                pass                     8                         pass
#> 12                pass                     8                         pass
#> 13                pass                     8                         pass
#> 14                pass                     8                         pass
#> 15                pass                     8                         pass
#> 16                pass                     8                         pass
#> 17                pass                     8                         pass
#> 18                pass                     8                         pass
#> 19                pass                     8                         pass
#> 20                pass                     8                         pass
#> 21                pass                     8                         pass
#> 22                pass                     8                         pass
#> 23                pass                     8                         pass
#> 24                pass                     8                         pass
#> 25                pass                     8                         pass
#> 26                pass                     8                         pass
#> 27                pass                     8                         pass
#> 28                pass                     8                         pass
#> 29                pass                     8                         pass
#> 30                pass                     8                         pass
#> 31                pass                     8                         pass
#> 32                pass                     8                         pass
#> 33                pass                     8                         pass
#> 34                pass                     8                         pass
#> 35                pass                     8                         pass
#> 36                pass                     8                         pass
#> 37                pass                     8                         pass
#> 38                pass                     8                         pass
#> 39                pass                     8                         pass
#> 40                pass                     8                         pass
#> 41                pass                     8                         pass
#> 42                pass                     8                         pass
#> 43                pass                     8                         pass
#> 44                pass                     8                         pass
#> 45                pass                     8                         pass
#> 46                pass                     8                         pass
#> 47                pass                     8                         pass
#> 48                pass                     8                         pass
#>         MC   MedC Negative_factor Background n_flags status reason
#> 1  2049.32  672.0       16.663102         NA       0   pass   <NA>
#> 2  1567.46  988.0        9.604329         NA       0   pass   <NA>
#> 3   918.48  611.0        9.716379         NA       0   pass   <NA>
#> 4   758.02  333.0       10.457253         NA       0   pass   <NA>
#> 5  2711.44  366.0       15.492908         NA       0   pass   <NA>
#> 6  1493.18  459.5       13.464436         NA       0   pass   <NA>
#> 7   516.32  240.5       10.333635         NA       0   pass   <NA>
#> 8   851.64  351.0        7.062781         NA       0   pass   <NA>
#> 9  2859.06  387.0       11.392962         NA       0   pass   <NA>
#> 10 2487.26  790.0       16.506374         NA       0   pass   <NA>
#> 11 1413.62  485.0       14.329145         NA       0   pass   <NA>
#> 12 1312.64  482.5       11.737886         NA       0   pass   <NA>
#> 13  526.20  320.0        8.467141         NA       0   pass   <NA>
#> 14  846.42  317.5        6.231753         NA       0   pass   <NA>
#> 15  604.06  435.5        6.197825         NA       0   pass   <NA>
#> 16  648.64  192.5        9.324605         NA       0   pass   <NA>
#> 17  897.30  277.5        8.016650         NA       0   pass   <NA>
#> 18 2180.28  878.5        9.722394         NA       0   pass   <NA>
#> 19 1352.90  592.5       10.249973         NA       0   pass   <NA>
#> 20  654.82  378.5        7.001347         NA       0   pass   <NA>
#> 21  665.72  510.5        8.717695         NA       0   pass   <NA>
#> 22 1667.60  391.5       14.832857         NA       0   pass   <NA>
#> 23  606.96  137.0        6.449516         NA       0   pass   <NA>
#> 24 1948.26  644.0        9.025430         NA       0   pass   <NA>
#> 25  313.58  118.5        6.769472         NA       0   pass   <NA>
#> 26  908.26  235.5       10.065594         NA       0   pass   <NA>
#> 27  433.16  157.0        5.309730         NA       0   pass   <NA>
#> 28 1218.74  302.0        8.019501         NA       0   pass   <NA>
#> 29 1425.84  636.0       10.487655         NA       0   pass   <NA>
#> 30 2186.20  327.0        9.619952         NA       0   pass   <NA>
#> 31  585.40  286.0        5.039188         NA       0   pass   <NA>
#> 32 2451.82  558.0       12.815770         NA       0   pass   <NA>
#> 33  765.04  521.0        8.740460         NA       0   pass   <NA>
#> 34 1063.74  266.5        9.822853         NA       0   pass   <NA>
#> 35  497.92  188.0        9.375881         NA       0   pass   <NA>
#> 36  832.12  478.5        4.367861         NA       0   pass   <NA>
#> 37 1104.90  431.5       10.407284         NA       0   pass   <NA>
#> 38  942.80  572.5        7.581896         NA       0   pass   <NA>
#> 39  615.32  204.0        8.756778         NA       0   pass   <NA>
#> 40  629.48  382.5       10.393900         NA       0   pass   <NA>
#> 41 1551.80  526.5       11.662690         NA       0   pass   <NA>
#> 42 3048.70  835.0       16.924784         NA       0   pass   <NA>
#> 43 1797.62  499.5        8.910512         NA       0   pass   <NA>
#> 44 1489.18  300.5       11.185507         NA       0   pass   <NA>
#> 45 1168.94  257.0       10.367145         NA       0   pass   <NA>
#> 46 1695.52  443.5       10.465828         NA       0   pass   <NA>
#> 47 1765.58  299.0       12.646193         NA       0   pass   <NA>
#> 48 2231.70 1315.0       14.287106         NA       0   pass   <NA>
```
