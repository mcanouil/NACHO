### NACHO

*NACHO* (**NA**noString quality **C**ontrol das**H**b**O**ard) works with NanoString nCounter data.
An nCounter assay measures messenger RNA and micro RNA (mRNA/miRNA) expression with fluorescent barcodes.
Each barcode is assigned to an mRNA or miRNA, and it is counted after it binds its target.
Each count of a barcode therefore shows how much of its target is present.

*NACHO* loads, visualizes and normalizes the exported nCounter data, and it helps you to check its quality.
It shows quality control metrics, the expression of control genes, principal components and sample-specific size factors in an interactive web application.

Two functions summarize and visualize the RCC files:

* The `load_rcc()` function preprocesses the data.
* The `visualise()` function starts a [Shiny-based dashboard](https://shiny.posit.co/) with all relevant QC plots.

*NACHO* also has a `normalise()` function.
It calculates the sample-specific size factors again and normalizes the data.

* The `normalise()` function returns a `nacho` object that holds your settings, the raw counts and the normalized counts.

Since v0.6.0, *NACHO* has two more functions:

* The `render()` function writes a full quality control report with Quarto, as an HTML or PDF file, from the result of `load_rcc()` or `normalise()`.
* The `autoplot()` function draws the quality control metrics of `visualise()` and `render()`.

For more information, see `vignette("NACHO")` and `vignette("NACHO-analysis")`.
