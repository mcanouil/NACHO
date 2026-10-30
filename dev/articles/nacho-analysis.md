# Analyse a GEO study

This article analyses a public NanoString study from start to finish.
GSE178516 measured 30 colorectal samples from patients with Lynch
syndrome on the PanCancer IO 360 panel. The samples fall into three
groups of 10: carcinoma, mucosa of healthy carriers and mucosa of tumour
patients. They ran on five cartridges. The article loads the RCC files,
checks that the design allows a comparison, and then tests the two
mucosa groups with *limma* and with
[`lm()`](https://rdrr.io/r/stats/lm.html).

``` r

library(NACHO)
library(limma)
```

## Download the study

The chunk below downloads the RCC files and the sample information from
GEO. It stops with a message when GEO cannot be reached. The sample
information comes from the series matrix. The three groups sit in its
`group:ch1` column.

``` r

series <- "GSE178516"
download_directory <- file.path(tempdir(), series)
rcc_directory <- file.path(download_directory, "RCC")

geo <- tryCatch(
  {
    series_matrix <- GEOquery::getGEO(series, destdir = tempdir())[[1]]
    GEOquery::getGEOSuppFiles(GEO = series, baseDir = tempdir())
    utils::untar(
      tarfile = file.path(download_directory, paste0(series, "_RAW.tar")),
      exdir = rcc_directory
    )
    series_matrix
  },
  error = function(error) {
    stop(
      "Cannot download ", series, " from GEO: ", conditionMessage(error),
      "\nCheck the internet connection and run the article again.",
      call. = FALSE
    )
  }
)
#> Setting options('download.file.method.GEOquery'='auto')
#> Setting options('GEOquery.inmemory.gpl'=FALSE)
#> Found 1 file(s)
#> GSE178516_series_matrix.txt.gz

rcc_files <- list.files(rcc_directory, pattern = "\\.RCC(\\.gz)?$", ignore.case = TRUE)
if (length(rcc_files) != 30) {
  stop(
    "Expected 30 RCC files from GSE178516, found ", length(rcc_files), ". ",
    "The GEO download may be incomplete; delete ", download_directory, " and build the article again.",
    call. = FALSE
  )
}
phenotypes <- Biobase::pData(geo)
samplesheet <- data.frame(
  IDFILE = rcc_files,
  geo_accession = sub("_.*$", "", rcc_files)
)
samplesheet[["group"]] <- c(
  "carcinoma (Lynch syndrome)" = "carcinoma",
  "mucosa (Lynch syndrome healthy carrier)" = "carrier mucosa",
  "mucosa (Lynch syndrome tumor patient)" = "patient mucosa"
)[phenotypes[samplesheet[["geo_accession"]], "group:ch1"]]
table(samplesheet[["group"]])
#> 
#>      carcinoma carrier mucosa patient mucosa 
#>             10             10             10
```

## Load the RCC files

[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
reads the 30 files, computes the quality-control metrics and normalises
the counts. The files do not name the instrument, so we say it is an
nCounter MAX.

``` r

x <- load_rcc(
  data_directory = rcc_directory,
  ssheet_csv = samplesheet,
  id_colname = "IDFILE",
  instrument = "max"
)
#> ℹ Reading 30 RCC files
#> ✔ Reading 30 RCC files [86ms]
#> 
#> ℹ Assembling 30 samples
#> ✔ Assembling 30 samples [29ms]
#> 
#> ℹ Computing quality-control metrics and normalising
#> ✔ Computing quality-control metrics and normalising [46ms]
#> 
print(x)
#> <nacho> 30 samples, 784 probes, from single-sample RCC files
#> Normalisation: GEO, with 20 housekeeping genes
#> Flagged samples: 2 of 30
#> Created with NACHO 2.0.8.9000
```

## Check the design

A comparison between groups only means something when the groups are
measured under the same technical conditions.
[`batch_diagnostics()`](https://m.canouil.dev/NACHO/dev/reference/batch_diagnostics.md)
crosses the groups with the cartridges, and tests whether the design is
confounded.

``` r

diagnostics <- batch_diagnostics(x, group = "group", batch = "CartridgeID")
diagnostics[["design"]]
#>         batch n_levels cramers_v single_group_levels confounded
#> 1 CartridgeID        5 0.7270292                   1       TRUE
diagnostics[["crosstabs"]][["CartridgeID"]]
#>                 CartridgeID
#> group            Lynch AG Kloor Lynch Lauf 1 Lynch Lauf 3 Lynch Lauf 4
#>   carcinoma                   0            0            0            0
#>   carrier mucosa              3            3            4            0
#>   patient mucosa              3            3            3            1
#>                 CartridgeID
#> group            Lynch Syndrom Lauf 5
#>   carcinoma                        10
#>   carrier mucosa                    0
#>   patient mucosa                    0
```

All 10 carcinoma samples ran on one cartridge, and no other sample did.
The `confounded` column is `TRUE` for that reason. A difference between
carcinoma and mucosa could come from the biology or from the cartridge,
and no normalisation can tell them apart. We do not compare the
carcinoma group with the others.

Carrier mucosa and patient mucosa are spread over several cartridges, so
the cartridge does not stand in for the group. We compare these two
groups.

## Test with limma

We keep the endogenous probes and the normalised counts of the mucosa
samples. The counts go to *limma* on the log2 scale.

``` r

mucosa <- nacho_samples(x)[["group"]] %in% c("carrier mucosa", "patient mucosa")
endogenous <- nacho_probes(x)[["CodeClass"]] == "Endogenous"
counts <- log2(nacho_counts(x, normalised = TRUE)[endogenous, mucosa] + 1)

groups <- factor(
  nacho_samples(x)[["group"]][mucosa],
  levels = c("carrier mucosa", "patient mucosa")
)
design <- model.matrix(~groups)
colnames(design) <- c("carrier", "patient_vs_carrier")

fit <- eBayes(lmFit(counts, design))
topTable(fit, coef = "patient_vs_carrier", number = 10)
#>               logFC  AveExpr         t      P.Value    adj.P.Val         B
#> DDB2      1.7250240 8.151740  9.793036 2.423960e-09 1.817970e-06 11.520074
#> WNT2B     1.1053938 7.907584  7.564270 1.819911e-07 5.994488e-05  7.415344
#> MYC       1.6045335 9.227140  7.402225 2.555571e-07 5.994488e-05  7.088585
#> NDUFA4L2  1.4269156 5.083612  7.223298 3.733021e-07 5.994488e-05  6.723327
#> DUSP2     1.9851700 4.615475  7.079311 5.079774e-07 5.994488e-05  6.426022
#> IKBKG     1.1078779 7.705575  7.000116 6.024718e-07 5.994488e-05  6.261231
#> HES1      0.8462820 9.776775  6.985388 6.219510e-07 5.994488e-05  6.230484
#> DUSP5     1.3678178 9.686291  6.972583 6.394121e-07 5.994488e-05  6.203728
#> TGFBR2   -0.9432864 8.149731 -6.827740 8.758913e-07 7.115540e-05  5.899462
#> ERO1A    -0.9619282 8.302934 -6.791168 9.487386e-07 7.115540e-05  5.822171
```

Each row is a probe. `logFC` is the difference in log2 counts between
patient mucosa and carrier mucosa. `adj.P.Val` is the p-value adjusted
for the number of probes tested.

## Test with lm

*limma* fits one model to all probes. For a single probe, or for any
other model, `as.data.frame(x, long = TRUE)` gives one row per probe and
sample. The example below fits a linear model for ten probes.

``` r

long <- as.data.frame(x, long = TRUE)
long <- long[
  long[["CodeClass"]] == "Endogenous" &
    long[["group"]] %in% c("carrier mucosa", "patient mucosa") &
    long[["Name"]] %in% head(unique(long[["Name"]]), 10),
]
fits <- lapply(split(long, long[["Name"]]), function(probe) {
  fit <- coef(summary(lm(log2(Count_Norm + 1) ~ group, data = probe)))
  data.frame(
    Name = probe[["Name"]][1],
    term = rownames(fit),
    fit,
    check.names = FALSE,
    row.names = NULL
  )
})
do.call(rbind, fits)
#>              Name                term    Estimate Std. Error     t value
#> A2M.1         A2M         (Intercept) 11.27131576 0.11713669  96.2236131
#> A2M.2         A2M grouppatient mucosa -0.75870567 0.16565630  -4.5799989
#> ACVR1C.1   ACVR1C         (Intercept)  6.43195021 0.13039924  49.3250579
#> ACVR1C.2   ACVR1C grouppatient mucosa  0.53534755 0.18441238   2.9029914
#> ADAM12.1   ADAM12         (Intercept)  3.34096786 0.19545349  17.0934159
#> ADAM12.2   ADAM12 grouppatient mucosa  0.78037493 0.27641298   2.8232210
#> ADGRE1.1   ADGRE1         (Intercept)  4.53171132 0.32226018  14.0622753
#> ADGRE1.2   ADGRE1 grouppatient mucosa -0.88010644 0.45574471  -1.9311391
#> ADM.1         ADM         (Intercept)  6.87687399 0.25469051  27.0009041
#> ADM.2         ADM grouppatient mucosa  0.97169644 0.36018677   2.6977572
#> ADORA2A.1 ADORA2A         (Intercept)  5.33329401 0.12665458  42.1089703
#> ADORA2A.2 ADORA2A grouppatient mucosa  0.32697062 0.17911663   1.8254622
#> AKT1.1       AKT1         (Intercept) 10.14519876 0.08731007 116.1973446
#> AKT1.2       AKT1 grouppatient mucosa  0.35328925 0.12347509   2.8612188
#> ALDOA.1     ALDOA         (Intercept) 12.63358280 0.05166335 244.5366392
#> ALDOA.2     ALDOA grouppatient mucosa  0.02681814 0.07306302   0.3670549
#> ALDOC.1     ALDOC         (Intercept)  6.61675731 0.21661669  30.5459250
#> ALDOC.2     ALDOC grouppatient mucosa -0.27673112 0.30634227  -0.9033397
#> ANGPT1.1   ANGPT1         (Intercept)  4.64944254 0.16549151  28.0947490
#> ANGPT1.2   ANGPT1 grouppatient mucosa  0.48280389 0.23404034   2.0629088
#>               Pr(>|t|)
#> A2M.1     7.235124e-26
#> A2M.2     2.322438e-04
#> ACVR1C.1  1.156484e-20
#> ACVR1C.2  9.483917e-03
#> ADAM12.1  1.421657e-12
#> ADAM12.2  1.126161e-02
#> ADGRE1.1  3.777240e-11
#> ADGRE1.2  6.936907e-02
#> ADM.1     5.132728e-16
#> ADM.2     1.472161e-02
#> ADORA2A.1 1.947509e-19
#> ADORA2A.2 8.457028e-02
#> AKT1.1    2.439208e-27
#> AKT1.2    1.037808e-02
#> ALDOA.1   3.753350e-33
#> ALDOA.2   7.178578e-01
#> ALDOC.1   5.829269e-17
#> ALDOC.2   3.782765e-01
#> ANGPT1.1  2.551192e-16
#> ANGPT1.2  5.385259e-02
```
