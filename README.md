
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Nanostring Quality Control Dashboard <img src="man/figures/nacho_hex.png" align="right" width="120" />

[![Travis-CI Build
Status](https://travis-ci.org/mcanouil/NACHO.svg?branch=master)](https://travis-ci.org/mcanouil/NACHO)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/mcanouil/NACHO?branch=master&svg=true)](https://ci.appveyor.com/project/mcanouil/NACHO)
[![Coverage Status
(codecov)](https://codecov.io/gh/mcanouil/NACHO/branch/master/graph/badge.svg)](https://codecov.io/gh/mcanouil/NACHO)
[![Coverage Status
(coveralls)](https://coveralls.io/repos/github/mcanouil/NACHO/badge.svg?branch=master)](https://coveralls.io/github/mcanouil/NACHO?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/NACHO)](https://cran.r-project.org/package=NACHO)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/NACHO)](https://cran.r-project.org/package=NACHO)

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
NanoString nCounter data is a mRNA or miRNA expression assay and works
with fluorescent barcodes.  
Each barcode is assigned a mRNA/miRNA, which can be counted after
bonding with its target.  
As a result each count of a specific barcode represents the presence of
its target mRNA/miRNA.

*NACHO* is able to load, visualise and normalise the exported NanoString
nCounter data and facilitates the user in performing a quality
control.  
*NACHO* does this by visualising Quality Control metrics, expression of
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

For more `vignette("NACHO")`
