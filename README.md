
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Nanostring Quality Control Dashboard <img src="man/figures/nacho_hex.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/NACHO.svg?label=%22latest%20tag%22)](https://github.com/mcanouil/NACHO)
[![Travis-CI Build
Status](https://travis-ci.org/mcanouil/NACHO.svg?branch=master)](https://travis-ci.org/mcanouil/NACHO)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/mcanouil/NACHO?branch=master&svg=true)](https://ci.appveyor.com/project/mcanouil/NACHO)
[![Coverage Status
(codecov)](https://codecov.io/gh/mcanouil/NACHO/branch/master/graph/badge.svg)](https://codecov.io/gh/mcanouil/NACHO)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/2719/badge)](https://bestpractices.coreinfrastructure.org/projects/2719)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/NACHO)](https://cran.r-project.org/package=NACHO)
[![cran
checks\_worst](https://cranchecks.info/badges/worst/NACHO)](https://cran.r-project.org/web/checks/check_results_NACHO.html)
[![CRAN\_Download\_total](http://cranlogs.r-pkg.org/badges/grand-total/NACHO)](https://cran.r-project.org/package=NACHO)
<!--[![cran checks_summary](https://cranchecks.info/badges/summary/NACHO)](https://cran.r-project.org/web/checks/check_results_NACHO.html)-->
<!--[![CRAN_Download_month](http://cranlogs.r-pkg.org/badges/NACHO?color=brightgreen)](https://cran.r-project.org/package=NACHO)-->
<!--[![Coverage Status (coveralls)](https://coveralls.io/repos/github/mcanouil/NACHO/badge.svg?branch=master)](https://coveralls.io/github/mcanouil/NACHO?branch=master)-->
<!-- badges: end -->

## Installation

``` r
# Install NACHO from CRAN:
install.packages("NACHO")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("mcanouil/NACHO")
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
namely: `summarise()` and `visualise()`.

  - The `summarise()` function is used to preprocess the data.
  - The `visualise()` function initiates a RStudio Shiny-based dashboard
    that visualises all relevant QC plots.

*NACHO* also includes a function `normalise()`, which calculates sample
specific size factors and normalises the data.

  - The `normalise()` function creates a list in which your settings,
    the raw counts and normalised counts are stored.

In addition (since v0.6.0) **NACHO** allows to render (`render()`) a
full quality-control report based on the results of a call to
`summarise()` or `normalise()`.

For more `vignette("NACHO")`

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/mcanouil/NACHO/issues).  
For questions and other discussion, please contact the package
maintainer.

-----

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md).  
By participating in this project you agree to abide by its terms.
