# Quality-control thresholds

Builds the thresholds that
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
and
[`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
use to flag samples.

## Usage

``` r
nacho_thresholds(
  instrument = c("max", "flex", "pro", "sprint"),
  preset = c("nsolver", "legacy"),
  haemolysis = FALSE
)
```

## Arguments

- instrument:

  The nCounter instrument: `"max"`, `"flex"`, `"pro"` or `"sprint"`.

- preset:

  `"nsolver"` or `"legacy"`.

- haemolysis:

  If `TRUE`, flag samples of a miRNA panel whose `miR-451a` to
  `miR-23a-3p` log2 ratio is above 7, a sign of haemolysis in plasma and
  serum.
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  does not take it, so pass the thresholds to
  `normalise(outliers_thresholds = )`.

## Value

A named list: `preset`, `instrument`, then one element per metric.

## Details

`"nsolver"` follows the current Bruker nCounter knowledge base
(<https://brukerspatialbiology.com/support/>, "nCounter QC flags"):
binding density 0.05 to 2.25 on MAX, FLEX and PRO and 0.1 to 1.8 on
SPRINT, field of view at least 75 %, positive control linearity at least
0.95 (POS_A to POS_E, with `log2(count + 1)`), limit of detection at
least 2, positive factor 0.3 to 3 and housekeeping factor 0.1 to 10. It
also asks for at least three housekeeping genes above background, as
Bruker asks (`Housekeeping_detected`).

`"legacy"` reproduces NACHO 2: binding density 0.1 to 2.25 on every
instrument, positive control linearity with POS_F, positive factor 1/4
to 4 and housekeeping factor 1/11 to 11. With
`background = "geo", background_mode = "subtract"`, it gives back the
NACHO 2 outlier calls.

The preset also chooses how negative probes are excluded and how the
positive control linearity is computed, so changing it normalises again.

For miRNA panels, `Ligation_order` (the three ligation positive controls
in decreasing order), `Ligation_R2` (their log2 counts on a line) and
`Ligation_NEG` (the largest ligation negative minus the detection limit)
are NACHO's own definitions, since Bruker publishes no threshold.
`"legacy"` never flags them.

Whole-blood RNA raises `LIG_NEG_A`, and to a lesser extent `LIG_NEG_B`,
together with red blood cell miRNAs such as `miR-451a`. The ligation
negatives of such samples are above the detection limit, so most samples
of a whole-blood study fail `Ligation_NEG`. This pattern occurs in
independent PAXgene whole-blood studies on GEO (GSE303669 and
GSE317496), and not in plasma, serum or extracellular vesicle studies.
When more than half of the samples fail `Ligation_NEG`,
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
warns about this pattern. For whole-blood samples, set a higher upper
bound for `Ligation_NEG` and pass the thresholds to
`normalise(outliers_thresholds = )`. Do not remove these samples with
[`exclude_outliers()`](https://m.canouil.dev/NACHO/dev/reference/exclude_outliers.md)
because of this flag only.

Each element can be changed afterwards; two numbers are a lower and an
upper bound, and one number is a lower bound. Use `-Inf` or `Inf` for an
open bound.

## Examples

``` r
nacho_thresholds()
#> $preset
#> [1] "nsolver"
#> 
#> $instrument
#> [1] "max"
#> 
#> $BD
#> [1] 0.05 2.25
#> 
#> $FoV
#> [1] 75
#> 
#> $PCL
#> [1] 0.95
#> 
#> $LoD
#> [1] 2
#> 
#> $Positive_factor
#> [1] 0.3 3.0
#> 
#> $House_factor
#> [1]  0.1 10.0
#> 
#> $Housekeeping_detected
#> [1] 3
#> 
#> $Ligation_order
#> [1] 1
#> 
#> $Ligation_R2
#> [1] 0.95
#> 
#> $Ligation_NEG
#> [1] -Inf    0
#> 
#> $Haemolysis
#> [1] -Inf  Inf
#> 
nacho_thresholds("sprint")
#> $preset
#> [1] "nsolver"
#> 
#> $instrument
#> [1] "sprint"
#> 
#> $BD
#> [1] 0.1 1.8
#> 
#> $FoV
#> [1] 75
#> 
#> $PCL
#> [1] 0.95
#> 
#> $LoD
#> [1] 2
#> 
#> $Positive_factor
#> [1] 0.3 3.0
#> 
#> $House_factor
#> [1]  0.1 10.0
#> 
#> $Housekeeping_detected
#> [1] 3
#> 
#> $Ligation_order
#> [1] 1
#> 
#> $Ligation_R2
#> [1] 0.95
#> 
#> $Ligation_NEG
#> [1] -Inf    0
#> 
#> $Haemolysis
#> [1] -Inf  Inf
#> 
nacho_thresholds(preset = "legacy")
#> $preset
#> [1] "legacy"
#> 
#> $instrument
#> [1] "max"
#> 
#> $BD
#> [1] 0.10 2.25
#> 
#> $FoV
#> [1] 75
#> 
#> $PCL
#> [1] 0.95
#> 
#> $LoD
#> [1] 2
#> 
#> $Positive_factor
#> [1] 0.25 4.00
#> 
#> $House_factor
#> [1]  0.09090909 11.00000000
#> 
#> $Housekeeping_detected
#> [1] 0
#> 
#> $Ligation_order
#> [1] 0
#> 
#> $Ligation_R2
#> [1] 0
#> 
#> $Ligation_NEG
#> [1] -Inf  Inf
#> 
#> $Haemolysis
#> [1] -Inf  Inf
#> 
```
