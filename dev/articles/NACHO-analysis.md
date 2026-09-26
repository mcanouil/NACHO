# NACHO Analysis

![NACHO hexagonal logo.](nacho_hex.png)

## Installation

``` r

# Install NACHO from CRAN:
install.packages("NACHO")

# Or the development version from GitHub:
# install.packages("pak")
pak::pak("mcanouil/NACHO")
```

## Overview

*NACHO* (**NA**noString quality **C**ontrol das**H**b**O**ard) is
developed for NanoString nCounter data.  
NanoString nCounter data is a messenger-RNA/micro-RNA (mRNA/miRNA)
expression assay and works with fluorescent barcodes.  
Each barcode is assigned an mRNA/miRNA, which can be counted after
bonding with its target.  
As a result each count of a specific barcode represents the presence of
its target mRNA/miRNA.

*NACHO* is able to load, visualise and normalise the exported NanoString
nCounter data and facilitates the user in performing a quality
control.  
*NACHO* does this by visualising quality control metrics, expression of
control genes, principal components and sample specific size factors in
an interactive web application.

With the use of two functions, RCC files are summarised and visualised,
namely:
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
and
[`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md).

- The
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  function is used to preprocess the data.
- The
  [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
  function initiates a [Shiny-based dashboard](https://shiny.posit.co/)
  that visualises all relevant QC plots.

*NACHO* also includes a function
[`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md),
which (re)calculates sample specific size factors and normalises the
data.

- The
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  function creates a list in which your settings, the raw counts and
  normalised counts are stored.

In addition (since v0.6.0) *NACHO* includes two (three) additional
functions:

- The [`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md)
  function renders a full quality-control report (HTML) based on the
  results of a call to
  [`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
  or
  [`normalise()`](https://m.canouil.dev/NACHO/dev/reference/normalise.md)
  (using [`print()`](https://rdrr.io/r/base/print.html) in an R Markdown
  chunk).
- The
  [`autoplot()`](https://m.canouil.dev/NACHO/dev/reference/autoplot.md)
  function draws any quality-control metrics from
  [`visualise()`](https://m.canouil.dev/NACHO/dev/reference/visualise.md)
  and [`render()`](https://m.canouil.dev/NACHO/dev/reference/render.md).

For more
[`vignette("NACHO")`](https://m.canouil.dev/NACHO/dev/articles/NACHO.md)
and
[`vignette("NACHO-analysis")`](https://m.canouil.dev/NACHO/dev/articles/NACHO-analysis.md).

Canouil M, Bouland GA, Bonnefond A, Froguel P, ’t Hart LM, Slieker RC
(2019). “NACHO: an R package for quality control of NanoString nCounter
data.” *Bioinformatics*. ISSN 1367-4803.
[doi:10.1093/bioinformatics/btz647](https://doi.org/10.1093/bioinformatics/btz647).

    @Article{,
      title = {{NACHO}: an {R} package for quality control of {NanoString} {nCounter} data},
      author = {Mickaël Canouil and Gerard A. Bouland and Amélie Bonnefond and Philippe Froguel and Leen M. {'t Hart} and Roderick C. Slieker},
      journal = {Bioinformatics},
      address = {Oxford, England},
      year = {2019},
      month = {aug},
      issn = {1367-4803},
      doi = {10.1093/bioinformatics/btz647},
    }

## Analyse NanoString data

### Load packages

``` r

library(NACHO)
library(data.table)
## 
## Attaching package: 'data.table'
## The following object is masked from 'package:base':
## 
##     %notin%
library(GEOquery, quietly = TRUE, warn.conflicts = FALSE)
## 
## Attaching package: 'generics'
## The following object is masked from 'package:NACHO':
## 
##     visualize
## The following objects are masked from 'package:base':
## 
##     as.difftime, as.factor, as.ordered, intersect, is.element, setdiff,
##     setequal, union
## 
## Attaching package: 'BiocGenerics'
## The following object is masked from 'package:NACHO':
## 
##     normalize
## The following objects are masked from 'package:stats':
## 
##     IQR, mad, sd, var, xtabs
## The following objects are masked from 'package:base':
## 
##     anyDuplicated, aperm, append, as.data.frame, basename, cbind,
##     colnames, dirname, do.call, duplicated, eval, evalq, Filter, Find,
##     get, grep, grepl, is.unsorted, lapply, Map, mapply, match, mget,
##     order, paste, pmax, pmax.int, pmin, pmin.int, Position, rank,
##     rbind, Reduce, rownames, sapply, saveRDS, table, tapply, unique,
##     unsplit, which.max, which.min
## Welcome to Bioconductor
## 
##     Vignettes contain introductory material; view with
##     'browseVignettes()'. To cite Bioconductor, see
##     'citation("Biobase")', and for packages 'citation("pkgname")'.
## Warning: replacing previous import 'S4Arrays::makeNindexFromArrayViewport' by
## 'DelayedArray::makeNindexFromArrayViewport' when loading 'SummarizedExperiment'
## Setting options('download.file.method.GEOquery'='auto')
## Setting options('GEOquery.inmemory.gpl'=FALSE)
```

### Download `GSE70970` from GEO (or use your own data)

The samples in GSE70970 were measured with two versions of the miRNA
CodeSet. Each CodeSet was exported by a different nSolver version, and
[`load_rcc()`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md)
refuses files that mix versions, so the code keeps the samples measured
with `NS_H_miR_1.4`.

``` r

data_directory <- file.path(tempdir(), "GSE70970", "Data")

# Download data
gse <- getGEO("GSE70970")
## Found 1 file(s)
## GSE70970_series_matrix.txt.gz
getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
##                                                                    size isdir
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       1986560 FALSE
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz     672 FALSE
##                                                                 mode
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                        644
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz  644
##                                                                               mtime
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       2026-09-26 13:27:50
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz 2026-09-26 13:27:50
##                                                                               ctime
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       2026-09-26 13:27:50
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz 2026-09-26 13:27:50
##                                                                               atime
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       2026-09-26 13:27:50
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz 2026-09-26 13:27:50
##                                                                  uid  gid
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       1001 1001
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz 1001 1001
##                                                                  uname grname
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       runner runner
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz runner runner
##                                                                                                  fname
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                                             GSE70970_RAW.tar
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz GSE70970_characteristics_readme.txt.gz
##                                                                                  destdir
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       /tmp/RtmpBaXpkF/GSE70970
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz /tmp/RtmpBaXpkF/GSE70970
##                                                                                                                        filepath
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                                             /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz
##                                                                      GEO
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_RAW.tar                       GSE70970
## /tmp/RtmpBaXpkF/GSE70970/GSE70970_characteristics_readme.txt.gz GSE70970
# Unzip data
untar(
  tarfile = file.path(tempdir(), "GSE70970", "GSE70970_RAW.tar"),
  exdir = data_directory
)
# Get phenotypes and add IDs
targets <- pData(phenoData(gse[[1]]))
rcc_files <- list.files(data_directory, pattern = "\\.RCC(\\.gz)?$", ignore.case = TRUE)
targets$IDFILE <- rcc_files[match(targets$geo_accession, sub("_.*", "", rcc_files))]
targets <- targets[!is.na(targets$IDFILE), ]
# Keep the samples measured with the same CodeSet
codeset <- vapply(
  X = file.path(data_directory, targets$IDFILE),
  FUN = function(file) {
    header <- grep("^GeneRLF,", readLines(file, n = 40), value = TRUE)
    if (length(header) == 0) NA_character_ else header[1]
  },
  FUN.VALUE = character(1)
)
targets <- targets[codeset %in% "GeneRLF,NS_H_miR_1.4", ]
```

### Import RCC files

``` r

GSE70970 <- load_rcc(data_directory, targets, id_colname = "IDFILE")
## [NACHO] Importing RCC files.
## [NACHO] Performing QC and formatting data.
## [NACHO] Computing normalisation factors using "GEO" method.
## [NACHO] Missing values have been replaced with zeros for PCA.
## [NACHO] Normalising data using "GEO" method with housekeeping genes.
## [NACHO] Returning a list.
##   $ access              : character
##   $ housekeeping_genes  : character
##   $ housekeeping_predict: logical
##   $ housekeeping_norm   : logical
##   $ normalisation_method: character
##   $ remove_outliers     : logical
##   $ n_comp              : numeric
##   $ data_directory      : character
##   $ pc_sum              : data.frame
##   $ nacho               : data.frame
##   $ outliers_thresholds : list
```

### Perform the analyses using `limma`

``` r

library(limma)
## 
## Attaching package: 'limma'
## The following object is masked from 'package:BiocGenerics':
## 
##     plotMA
```

#### Get the phenotypes

``` r

selected_pheno <- GSE70970[["nacho"]][
  j = lapply(unique(.SD), function(x) ifelse(x == "NA", NA, x)),
  .SDcols = c("IDFILE", "age:ch1", "gender:ch1", "chemo:ch1", "disease.event:ch1")
]
selected_pheno <- na.exclude(selected_pheno)
```

    ##                         IDFILE     age:ch1 gender:ch1 chemo:ch1
    ##                         <char>      <char>     <char>    <char>
    ## 1:   GSM1824143_NPC-T-1.RCC.gz 45.97260274       Male         0
    ## 2:  GSM1824144_NPC-T-10.RCC.gz        46.4       Male         1
    ## 3: GSM1824145_NPC-T-100.RCC.gz 50.36438356       Male         0
    ## 4: GSM1824146_NPC-T-101.RCC.gz 64.09041096     Female         1
    ## 5: GSM1824147_NPC-T-102.RCC.gz 27.57808219       Male         1
    ## 6: GSM1824148_NPC-T-103.RCC.gz 67.01369863       Male         1
    ##    disease.event:ch1
    ##               <char>
    ## 1:                 1
    ## 2:                 1
    ## 3:                 0
    ## 4:                 0
    ## 5:                 1
    ## 6:                 0

#### Get the normalised counts

``` r

expr_counts <- GSE70970[["nacho"]][
  i = grepl("Endogenous", CodeClass),
  j = as.matrix(
    dcast(.SD, Name ~ IDFILE, value.var = "Count_Norm"),
    "Name"
  ),
  .SDcols = c("IDFILE", "Name", "Count_Norm")
]
```

    ##                             GSM1824143_NPC-T-1.RCC.gz
    ## bkv-miR-B1-3p+jcv-miR-J1-3p                       4.0
    ## bkv-miR-B1-5p                                     0.1
    ## ebv-miR-BART1-3p                               3526.0
    ## ebv-miR-BART1-5p                                196.0
    ## ebv-miR-BART10                                22936.0
    ##                             GSM1824144_NPC-T-10.RCC.gz
    ## bkv-miR-B1-3p+jcv-miR-J1-3p                       20.0
    ## bkv-miR-B1-5p                                      0.1
    ## ebv-miR-BART1-3p                                2799.0
    ## ebv-miR-BART1-5p                                  95.0
    ## ebv-miR-BART10                                  5833.0
    ##                             GSM1824145_NPC-T-100.RCC.gz
    ## bkv-miR-B1-3p+jcv-miR-J1-3p                          49
    ## bkv-miR-B1-5p                                        13
    ## ebv-miR-BART1-3p                                   1627
    ## ebv-miR-BART1-5p                                     92
    ## ebv-miR-BART10                                     3717
    ##                             GSM1824146_NPC-T-101.RCC.gz
    ## bkv-miR-B1-3p+jcv-miR-J1-3p                         7.0
    ## bkv-miR-B1-5p                                       0.1
    ## ebv-miR-BART1-3p                                 2880.0
    ## ebv-miR-BART1-5p                                   73.0
    ## ebv-miR-BART10                                   5084.0
    ##                             GSM1824147_NPC-T-102.RCC.gz
    ## bkv-miR-B1-3p+jcv-miR-J1-3p                         0.1
    ## bkv-miR-B1-5p                                       0.1
    ## ebv-miR-BART1-3p                                 4877.0
    ## ebv-miR-BART1-5p                                   44.0
    ## ebv-miR-BART10                                   6195.0

Alternatively, `"Accession"` number is also available.

``` r

GSE70970[["nacho"]][
  i = grepl("Endogenous", CodeClass),
  j = as.matrix(
    dcast(.SD, Accession ~ IDFILE, value.var = "Count_Norm"),
    "Accession"
  ),
  .SDcols = c("IDFILE", "Accession", "Count_Norm")
]
```

#### Select phenotypes and counts

1.  Make sure count matrix and phenotypes have the same samples

``` r

samples_kept <- intersect(selected_pheno[["IDFILE"]], colnames(expr_counts))
expr_counts <- expr_counts[, samples_kept]
selected_pheno <- selected_pheno[IDFILE %in% c(samples_kept)]
```

2.  Build the numeric design matrix

``` r

design <- model.matrix(~ `disease.event:ch1`, selected_pheno)
```

3.  `limma`

``` r

eBayes(lmFit(expr_counts, design))
## An object of class "MArrayLM"
## $coefficients
##                             (Intercept) `disease.event:ch1`1
## bkv-miR-B1-3p+jcv-miR-J1-3p   17.138372            -2.843500
## bkv-miR-B1-5p                  6.459302             2.961210
## ebv-miR-BART1-3p            3133.732558           211.446929
## ebv-miR-BART1-5p             197.898837             5.365265
## ebv-miR-BART10              7537.651163          2137.528324
## 730 more rows ...
## 
## $stdev.unscaled
##                             (Intercept) `disease.event:ch1`1
## bkv-miR-B1-3p+jcv-miR-J1-3p   0.1078328            0.1930516
## bkv-miR-B1-5p                 0.1078328            0.1930516
## ebv-miR-BART1-3p              0.1078328            0.1930516
## ebv-miR-BART1-5p              0.1078328            0.1930516
## ebv-miR-BART10                0.1078328            0.1930516
## 730 more rows ...
## 
## $sigma
## [1]    29.29642    13.41464  4850.90726   351.44148 10106.31634
## 730 more elements ...
## 
## $df.residual
## [1] 123 123 123 123 123
## 730 more elements ...
## 
## $cov.coefficients
##                      (Intercept) `disease.event:ch1`1
## (Intercept)           0.01162791          -0.01162791
## `disease.event:ch1`1 -0.01162791           0.03726893
## 
## $pivot
## [1] 1 2
## 
## $rank
## [1] 2
## 
## $Amean
## bkv-miR-B1-3p+jcv-miR-J1-3p               bkv-miR-B1-5p 
##                     16.2512                      7.3832 
##            ebv-miR-BART1-3p            ebv-miR-BART1-5p 
##                   3199.7040                    199.5728 
##              ebv-miR-BART10 
##                   8204.5600 
## 730 more elements ...
## 
## $method
## [1] "ls"
## 
## $design
##   (Intercept) `disease.event:ch1`1
## 1           1                    1
## 2           1                    1
## 3           1                    0
## 4           1                    0
## 5           1                    1
## 120 more rows ...
## 
## $df.prior
## [1] 0.5116399
## 
## $s2.prior
## [1] 638.9296
## 
## $var.prior
## [1] 2.504188e-02 1.565118e-05
## 
## $proportion
## [1] 0.01
## 
## $s2.post
## [1] 8.573719e+02 1.818537e+02 2.343383e+07 1.230021e+05 1.017145e+08
## 730 more elements ...
## 
## $t
##                             (Intercept) `disease.event:ch1`1
## bkv-miR-B1-3p+jcv-miR-J1-3p    5.427929          -0.50303151
## bkv-miR-B1-5p                  4.441951           1.13745622
## ebv-miR-BART1-3p               6.003293           0.22625923
## ebv-miR-BART1-5p               5.232824           0.07924309
## ebv-miR-BART10                 6.930966           1.09785983
## 730 more rows ...
## 
## $df.total
## [1] 123.5116 123.5116 123.5116 123.5116 123.5116
## 730 more elements ...
## 
## $p.value
##                              (Intercept) `disease.event:ch1`1
## bkv-miR-B1-3p+jcv-miR-J1-3p 2.895225e-07            0.6158380
## bkv-miR-B1-5p               1.955768e-05            0.2575494
## ebv-miR-BART1-3p            1.996900e-08            0.8213738
## ebv-miR-BART1-5p            6.931537e-07            0.9369675
## ebv-miR-BART10              2.049447e-10            0.2744014
## 730 more rows ...
## 
## $lods
##                             (Intercept) `disease.event:ch1`1
## bkv-miR-B1-3p+jcv-miR-J1-3p   3.6097221            -4.595276
## bkv-miR-B1-5p                 0.9808053            -4.595059
## ebv-miR-BART1-3p              5.2607434            -4.595319
## ebv-miR-BART1-5p              3.0672696            -4.595328
## ebv-miR-BART10                8.0438100            -4.595077
## 730 more rows ...
## 
## $F
## [1] 19.37878 19.38162 27.33149 20.24126 41.96523
## 730 more elements ...
## 
## $F.p.value
## [1] 4.788440e-08 4.778104e-08 1.486979e-10 2.492262e-08 1.239564e-14
## 730 more elements ...
```

### Perform the analyses using `lm` (or any other model)

``` r

GSE70970[["nacho"]][
  i = grepl("Endogenous", CodeClass),
  j = lapply(unique(.SD), function(x) ifelse(x == "NA", NA, x)),
  .SDcols = c(
    "IDFILE", "Name", "Accession", "Count", "Count_Norm",
    "age:ch1", "gender:ch1", "chemo:ch1", "disease.event:ch1"
  )
][
  Name %in% head(unique(Name), 10)
][
  j = as.data.table(
    coef(summary(lm(
      formula = Count_Norm ~ `disease.event:ch1`,
      data = na.exclude(.SD)
    ))),
    "term"
  ),
  by = c("Name", "Accession")
]
##             Name    Accession                 term      Estimate Std. Error
##           <char>       <char>               <char>         <num>      <num>
##  1:  hsa-miR-758 MIMAT0003879          (Intercept)   17.68255814   3.147292
##  2:  hsa-miR-758 MIMAT0003879 `disease.event:ch1`1   -1.33896840   5.634557
##  3: hsa-miR-1296 MIMAT0005794          (Intercept)    4.79418605   1.006124
##  4: hsa-miR-1296 MIMAT0005794 `disease.event:ch1`1   -0.06598092   1.801250
##  5: hsa-miR-548e MIMAT0005874          (Intercept)   11.71395349   2.760618
##  6: hsa-miR-548e MIMAT0005874 `disease.event:ch1`1   -2.88574836   4.942300
##  7:  hsa-miR-874 MIMAT0004911          (Intercept)   45.03604651   6.305683
##  8:  hsa-miR-874 MIMAT0004911 `disease.event:ch1`1   21.01779964  11.288983
##  9: hsa-miR-106b MIMAT0000680          (Intercept) 2707.67441860 256.404262
## 10: hsa-miR-106b MIMAT0000680 `disease.event:ch1`1  627.19737627 459.037265
## 11: hsa-miR-1825 MIMAT0006765          (Intercept)   33.87325581   5.550548
## 12: hsa-miR-1825 MIMAT0006765 `disease.event:ch1`1    2.97802624   9.937075
## 13: hsa-miR-133a MIMAT0000427          (Intercept)   40.52441860  13.164696
## 14: hsa-miR-133a MIMAT0000427 `disease.event:ch1`1   17.55763268  23.568586
## 15:  hsa-miR-203 MIMAT0000264          (Intercept)  584.37209302 241.138855
## 16:  hsa-miR-203 MIMAT0000264 `disease.event:ch1`1  747.44841980 431.707801
## 17:  hsa-miR-222 MIMAT0000279          (Intercept) 3400.08139535 378.018063
## 18:  hsa-miR-222 MIMAT0000279 `disease.event:ch1`1 -326.59421586 676.760893
## 19: hsa-miR-1973 MIMAT0009448          (Intercept)  276.46511628  45.990903
## 20: hsa-miR-1973 MIMAT0009448 `disease.event:ch1`1  211.45796064  82.336925
##             Name    Accession                 term      Estimate Std. Error
##           <char>       <char>               <char>         <num>      <num>
##         t value     Pr(>|t|)
##           <num>        <num>
##  1:  5.61833992 1.222143e-07
##  2: -0.23763507 8.125595e-01
##  3:  4.76500738 5.224757e-06
##  4: -0.03663063 9.708389e-01
##  5:  4.24323540 4.296251e-05
##  6: -0.58388778 5.603651e-01
##  7:  7.14213626 7.053164e-11
##  8:  1.86179739 6.501839e-02
##  9: 10.56017710 5.986499e-19
## 10:  1.36633216 1.743274e-01
## 11:  6.10268682 1.251554e-08
## 12:  0.29968842 7.649207e-01
## 13:  3.07826469 2.567805e-03
## 14:  0.74495909 4.577175e-01
## 15:  2.42338421 1.683288e-02
## 16:  1.73137575 8.589213e-02
## 17:  8.99449453 3.561126e-15
## 18: -0.48258435 6.302486e-01
## 19:  6.01129999 1.937975e-08
## 20:  2.56820328 1.141876e-02
##         t value     Pr(>|t|)
##           <num>        <num>
```
