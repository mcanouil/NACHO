
<!-- README.md is generated from README.Rmd. Please edit that file -->
NACHO <img src="man/figures/nacho_hex.png" align="right" width="120" />
=======================================================================

Overview
--------

*NACHO* (NanoString Quality Control Dashboard) is developed for NanoString nCounter data. NanoString nCounter data is a mRNA or miRNA expression assay and works with fluorescent barcodes. Each barcode is assigned a mRNA/miRNA, which can be counted after bonding with its target. As a result each count of a specific barcode represents the presence of its target mRNA/miRNA.

*NACHO* is able to load, visualise and normalise the exported NanoString nCounter data and facilitates the user in performing a quality control. *NACHO* does this by visualising Quality Control metrics, expression of control genes, principal components and sample specific size factors in an interactive web application. With the use of two functions, RCC files are summarised and visualised, namely: `summarise()` and `visualise()`.

-   The `summarise()` function is used to preprocess the data.
-   The `visualise()` function initiates a RStudio Shiny-based dashboard that visualises all relevant QC plots.

*NACHO* also includes a function `normalise()`, which calculates sample specific size factors and normalises the data.

-   The`normalise()` function creates a list in which your settings, the raw counts and normalised counts are stored.

Installation
------------

``` r
# The easiest way to get NACHO is to install the whole tidyverse:
install.packages("tidyverse")
# Alternatively, install just NACHO:
install.packages("NACHO")
# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("mcanouil/NACHO")
```
