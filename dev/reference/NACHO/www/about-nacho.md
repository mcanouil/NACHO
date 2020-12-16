### NACHO

*NACHO* (**NA**nostring quality **C**ontrol das**H**b**O**ard) is developed for NanoString nCounter data.  
NanoString nCounter data is a messenger-RNA/micro-RNA (mRNA/miRNA) expression assay and works with fluorescent barcodes.  
Each barcode is assigned a mRNA/miRNA, which can be counted after bonding with its target.  
As a result each count of a specific barcode represents the presence of its target mRNA/miRNA.

*NACHO* is able to load, visualise and normalise the exported NanoString nCounter data and facilitates the user in performing a quality control.  
*NACHO* does this by visualising quality control metrics, expression of control genes, principal components and sample specific size factors in an interactive web application.

With the use of two functions, RCC files are summarised and visualised, namely: `load_rcc()` and `visualise()`.

* The `load_rcc()` function is used to preprocess the data.
* The `visualise()` function initiates a [Shiny-based dashboard](https://shiny.rstudio.com/) that visualises all relevant QC plots.

*NACHO* also includes a function `normalise()`, which (re)calculates sample specific size factors and normalises the data.

* The `normalise()` function creates a list in which your settings, the raw counts and normalised counts are stored.

In addition (since v0.6.0) *NACHO* includes two (three) additional functions:

* The `render()` function renders a full quality-control report (HTML) based on the results of a call to `load_rcc()` or `normalise()` (using `print()` in a Rmarkdown chunk).
* The `autoplot()` function draws any quality-control metrics from `visualise()` and `render()`.

For more `vignette("NACHO")` and `vignette("NACHO-analysis")`.
