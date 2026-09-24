# NACHO Analysis

![](nacho_hex.png)

## Installation

``` r

# Install NACHO from CRAN:
install.packages("NACHO")

# Or the the development version from GitHub:
# install.packages("remotes")
remotes::install_github("mcanouil/NACHO")
```

## Overview

*NACHO* (**NA**nostring quality **C**ontrol das**H**b**O**ard) is
developed for NanoString nCounter data.  
NanoString nCounter data is a messenger-RNA/micro-RNA (mRNA/miRNA)
expression assay and works with fluorescent barcodes.  
Each barcode is assigned a mRNA/miRNA, which can be counted after
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
namely: [`load_rcc()`](../reference/load_rcc.md) and
[`visualise()`](../reference/visualise.md).

- The [`load_rcc()`](../reference/load_rcc.md) function is used to
  preprocess the data.
- The [`visualise()`](../reference/visualise.md) function initiates a
  [Shiny-based dashboard](https://shiny.posit.co/) that visualises all
  relevant QC plots.

*NACHO* also includes a function
[`normalise()`](../reference/normalise.md), which (re)calculates sample
specific size factors and normalises the data.

- The [`normalise()`](../reference/normalise.md) function creates a list
  in which your settings, the raw counts and normalised counts are
  stored.

In addition (since v0.6.0) *NACHO* includes two (three) additional
functions:

- The [`render()`](../reference/render.md) function renders a full
  quality-control report (HTML) based on the results of a call to
  [`load_rcc()`](../reference/load_rcc.md) or
  [`normalise()`](../reference/normalise.md) (using
  [`print()`](https://rdrr.io/r/base/print.html) in a Rmarkdown chunk).
- The [`autoplot()`](../reference/autoplot.md) function draws any
  quality-control metrics from
  [`visualise()`](../reference/visualise.md) and
  [`render()`](../reference/render.md).

For more [`vignette("NACHO")`](../articles/NACHO.md) and
[`vignette("NACHO-analysis")`](../articles/NACHO-analysis.md).

Canouil M, Bouland GA, Bonnefond A, Froguel P, Hart L, Slieker R (2019).
“NACHO: an R package for quality control of NanoString nCounter data.”
*Bioinformatics*. ISSN 1367-4803.
[doi:10.1093/bioinformatics/btz647](https://doi.org/10.1093/bioinformatics/btz647).

    @Article{,
      title = {{NACHO}: an {R} package for quality control of {NanoString} {nCounter} data},
      author = {Mickaël Canouil and Gerard A. Bouland and Amélie Bonnefond and Philippe Froguel and Leen Hart and Roderick Slieker},
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

``` r

data_directory <- file.path(tempdir(), "GSE70970", "Data")

# Download data
gse <- getGEO("GSE70970")
## Found 1 file(s)
## GSE70970_series_matrix.txt.gz
getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
##                                                                    size isdir
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       1986560 FALSE
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz     672 FALSE
##                                                                 mode
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                        644
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz  644
##                                                                               mtime
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       2026-09-24 19:44:11
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz 2026-09-24 19:44:11
##                                                                               ctime
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       2026-09-24 19:44:11
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz 2026-09-24 19:44:11
##                                                                               atime
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       2026-09-24 19:44:10
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz 2026-09-24 19:44:11
##                                                                  uid  gid
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       1001 1001
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz 1001 1001
##                                                                  uname grname
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       runner runner
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz runner runner
##                                                                                                  fname
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                                             GSE70970_RAW.tar
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz GSE70970_characteristics_readme.txt.gz
##                                                                                  destdir
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       /tmp/RtmphjNqrY/GSE70970
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz /tmp/RtmphjNqrY/GSE70970
##                                                                                                                        filepath
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                                             /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz
##                                                                      GEO
## /tmp/RtmphjNqrY/GSE70970/GSE70970_RAW.tar                       GSE70970
## /tmp/RtmphjNqrY/GSE70970/GSE70970_characteristics_readme.txt.gz GSE70970
# Unzip data
untar(
  tarfile = file.path(tempdir(), "GSE70970", "GSE70970_RAW.tar"),
  exdir = data_directory
)
# Get phenotypes and add IDs
targets <- pData(phenoData(gse[[1]]))
targets$IDFILE <- list.files(data_directory)
```

### Import RCC files

``` r

GSE70970 <- load_rcc(data_directory, targets, id_colname = "IDFILE")
## [NACHO] Importing RCC files.
## Error in `load_rcc()`:
## ! [NACHO] Multiple Nanostring file/software versions detected.
##   Please provide a set of files with the same version.
##   - FileVersion: '1.6', '1.6'
##   - SoftwareVersion: '2.1.2.3', '2.1.1.0005'
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
## Error:
## ! object 'GSE70970' not found
selected_pheno <- na.exclude(selected_pheno)
## Error:
## ! object 'selected_pheno' not found
```

    ## Error:
    ## ! object 'selected_pheno' not found

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
## Error:
## ! object 'GSE70970' not found
```

    ## Error:
    ## ! object 'expr_counts' not found

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
## Error in `h()`:
## ! error in evaluating the argument 'x' in selecting a method for function 'intersect': object 'selected_pheno' not found
expr_counts <- expr_counts[, samples_kept]
## Error:
## ! object 'expr_counts' not found
selected_pheno <- selected_pheno[IDFILE %in% c(samples_kept)]
## Error:
## ! object 'selected_pheno' not found
```

2.  Build the numeric design matrix

``` r

design <- model.matrix(~ `disease.event:ch1`, selected_pheno)
## Error:
## ! object 'selected_pheno' not found
```

3.  `limma`

``` r

eBayes(lmFit(expr_counts, design))
## Error:
## ! object 'expr_counts' not found
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
## Error:
## ! object 'GSE70970' not found
```
