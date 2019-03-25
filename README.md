
<!-- README.md is generated from README.Rmd. Please edit that file -->
NACHO <img src="man/figures/nacho_hex.png" align="right" width="120" />
=======================================================================

[![Travis-CI Build Status](https://travis-ci.org/mcanouil/NACHO.svg?branch=master)](https://travis-ci.org/mcanouil/NACHO) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mcanouil/NACHO?branch=master&svg=true)](https://ci.appveyor.com/project/mcanouil/NACHO) [![Coverage Status](https://img.shields.io/codecov/c/github/mcanouil/NACHO/master.svg)](https://codecov.io/github/mcanouil/NACHO?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/NACHO)](https://cran.r-project.org/package=NACHO) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/NACHO)](https://cran.r-project.org/package=NACHO)

Overview
--------

*NACHO* (NAnostring quality Control dasHbOard) is developed for NanoString nCounter data.
NanoString nCounter data is a mRNA or miRNA expression assay and works with fluorescent barcodes.
Each barcode is assigned a mRNA/miRNA, which can be counted after bonding with its target.
As a result each count of a specific barcode represents the presence of its target mRNA/miRNA.

*NACHO* is able to load, visualise and normalise the exported NanoString nCounter data and facilitates the user in performing a quality control.
*NACHO* does this by visualising Quality Control metrics, expression of control genes, principal components and sample specific size factors in an interactive web application.

With the use of two functions, RCC files are summarised and visualised, namely: `summarise()` and `visualise()`.

-   The `summarise()` function is used to preprocess the data.
-   The `visualise()` function initiates a RStudio Shiny-based dashboard that visualises all relevant QC plots.

*NACHO* also includes a function `normalise()`, which calculates sample specific size factors and normalises the data.

-   The `normalise()` function creates a list in which your settings, the raw counts and normalised counts are stored.

Installation
------------

``` r
# Install NACHO from CRAN:
install.packages("NACHO")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("mcanouil/NACHO")
```

An example
----------

To display the usage and utility of *NACHO*, we show three examples in which the above mentioned functions are used and the results are briefly examined. *NACHO* comes with presummarised data and in the first example we use this data to call the interactive web application with the use of `visualise()`. In the second example we show the process of going from raw RCC files to visualisations with a data set queried from **GEO** using `GEOquery`.

In the third example we use the summarised data from second example to calculate the sample specific size factors using `normalise()` and its added functionality to predict housekeeping genes.

Besides creating interactive visualisations, *NACHO* also identifies poorly performing samples which can be seen under the Outlier Table tab in the interactive web application.
While calling `normalise()` the user has the possibility to remove these outliers before size factor calculation.

### Get NanoString nCounter data

#### Presummarised data from *NACHO*

This example shows how to use summarised data to call the interactive web application.
The raw data used in the summarisation is from a study of Liu MC et al. \[1\] and was acquired from the NCBI GEO public database \[2\].

``` r
library(NACHO)
data(GSE74821)
visualise(GSE74821)
```

#### Raw data from GEO

Numerous NanoString nCounter data sets are available from GEO.
In this example we use a mRNA dataset from the study of *Bruce* et al. \[2\] with the GEO accession number: **GSE70970**. The data is extracted and prepared using the following code.

``` r
library(GEOquery)
# Download data
gse <- getGEO("GSE70970")
# Get phenotypes
targets <- pData(phenoData(gse[[1]]))
getGEOSuppFiles(GEO = "GSE70970", baseDir = tempdir())
# Unzip data
untar(
  tarfile = paste0(tempdir(), "/GSE70970/GSE70970_RAW.tar"), 
  exdir = paste0(tempdir(), "/GSE70970/Data")
)
# Add IDs
targets$IDFILE <- list.files(paste0(tempdir(), "/GSE70970/Data"))
```

After we extracted the dataset to the `paste0(tempdir(), "/GSE70970/Data")` directory, a `Samplesheet.csv` containing a column with the exact names of the files for each sample can be written or use as is.

### The `summarise()` function

The first argument requires the path to the directory containing the RCC files, the second argument is the location of samplesheet followed by third argument with the column name containing the exact names of the files.
The `housekeeping_genes` and `normalisation_method` arguments respectively indicate which housekeeping genes and normalisation method should be used.

``` r
library(NACHO)
#> 
#> Attaching package: 'NACHO'
#> The following object is masked from 'package:BiocGenerics':
#> 
#>     normalize
GSE70970_sum <- summarise(
  data_directory = paste0(tempdir(), "/GSE70970/Data"), # Where the data is
  ssheet_csv = targets, # The samplesheet
  id_colname = "IDFILE", # Name of the column that contains the identfiers
  housekeeping_genes = NULL, # Custom list of housekeeping genes
  housekeeping_predict = TRUE, # Predict the housekeeping genes based on the data?
  normalisation_method = "GEO", # Geometric mean or GLM
  n_comp = 5 # Number indicating the number of principal components to compute. 
)
#> [NACHO] Importing RCC files.
#> [NACHO] Performing QC and formatting data.
#> [NACHO] Searching for the best housekeeping genes.
#> [NACHO] Computing normalisation factors using "GEO" method for housekeeping genes prediction.
#> [NACHO] The following predicted housekeeping genes will be used for normalisation:
#>   - hsa-miR-103
#>   - hsa-let-7e
#>   - hsa-miR-1260
#>   - hsa-miR-500+hsa-miR-501-5p
#>   - hsa-miR-1274b
#> [NACHO] Computing normalisation factors using "GEO" method.
#> [NACHO] Missing values have been replaced with zeros for PCA.
#> [NACHO] Normalising data using "GEO" method with housekeeping genes.
#> [NACHO] Returning a list.
#>   $ access              : character
#>   $ housekeeping_genes  : character
#>   $ housekeeping_predict: logical
#>   $ housekeeping_norm   : logical
#>   $ normalisation_method: character
#>   $ remove_outliers     : logical
#>   $ n_comp              : numeric
#>   $ data_directory      : character
#>   $ pc_sum              : data.frame
#>   $ nacho               : data.frame
#>   $ outliers_thresholds : list
#>   $ raw_counts          : data.frame
#>   $ normalised_counts   : data.frame
```

### The `visualise()` function

When the summarisation is done, the summarised (or normalised) data can be visualised using the `visualise()` function as can be seen in the following chunk of code.

``` r
visualise(GSE70970_sum)
```

![](man/figures/README-visualise.png)

On the left there are widgets that control the interactive plotting area. These widgets differ dependent on which main tab is chosen. The second layer of tabs is also interactive and changes based on which main tab is chosen. Each sample in the plots can be coloured based on either technical specifications which are included in the RCC files or based on specifications of your own choosing, though these specifications need to be included in the samplesheet.

### The `normalise()` function

*NACHO* is allows the discovery of housekeeping genes within your own dataset. *NACHO* finds the five best suitable housekeeping genes, however, it is possible that one of these five genes might not be suitable, which is why a subset of these discovered housekeeping genes might work better in some cases. For this example we use the **GSE70970** dataset from the previous example. The discovered housekeeping genes are saved in a global variable named **predicted\_housekeeping**.

``` r
print(GSE70970_sum[["housekeeping_genes"]])
#> [1] "hsa-miR-103"                "hsa-let-7e"                
#> [3] "hsa-miR-1260"               "hsa-miR-500+hsa-miR-501-5p"
#> [5] "hsa-miR-1274b"
```

Let's say *hsa-miR-103* and *hsa-let-7e* are not suitable, therefore, you want to exclude these genes from the normalisation process.

``` r
my_housekeeping <- GSE70970_sum[["housekeeping_genes"]][-c(1, 2)]
print(my_housekeeping)
#> [1] "hsa-miR-1260"               "hsa-miR-500+hsa-miR-501-5p"
#> [3] "hsa-miR-1274b"
```

The next step is the actual normalisation. The first argument requires the summary which is created with the `summarise()` function. The second arugument requires a vector of gene names. In this case it is a subset of the discovered housekeeping genes we just made. With the third argument the user has the choice to remove the outliers. Lastly the normalisation method can be choosed.
Here the user has a choice between `"GLM"` or `"GEO"`. The differences between normalisation methods are nuanced, however, a preference for either method are use case specific.
In this example `"GLM"` is used.

``` r
GSE70970_norm <- normalise(
  nacho_object = GSE70970_sum,
  housekeeping_genes = my_housekeeping,
  housekeeping_norm = TRUE,
  normalisation_method = "GEO", 
  remove_outliers = TRUE
)
#> [NACHO] Normalising "GSE70970_sum" with new value for parameters:
#>   - housekeeping_genes = TRUE
#>   - remove_outliers = TRUE
#> [NACHO] Searching for the best housekeeping genes.
#> [NACHO] Computing normalisation factors using "GEO" method for housekeeping genes prediction.
#> [NACHO] The following predicted housekeeping genes will be used for normalisation:
#>   - hsa-let-7e
#>   - hsa-miR-1260
#>   - hsa-miR-1274b
#>   - hsa-miR-103
#>   - hsa-miR-16
#> [NACHO] Computing normalisation factors using "GEO" method.
#> [NACHO] Missing values have been replaced with zeros for PCA.
#> [NACHO] Returning a list.
#>   $ access              : character
#>   $ housekeeping_genes  : character
#>   $ housekeeping_predict: logical
#>   $ housekeeping_norm   : logical
#>   $ normalisation_method: character
#>   $ remove_outliers     : logical
#>   $ n_comp              : numeric
#>   $ data_directory      : character
#>   $ pc_sum              : data.frame
#>   $ nacho               : data.frame
#>   $ raw_counts          : data.frame
#>   $ normalised_counts   : data.frame
```

`normalise()` returns a `list` object (same as `summarise()`) with `raw_counts` and `normalised_counts` slots filled with the raw and normalised counts. Both counts are also in the *NACHO* data.frame.

References
----------

\[1\] Liu MC, Pitcher BN, Mardis ER, Davies SR, Friedman PN, Snider JE, Vickery TL, Reed JP, DeSchryver K, Singh B, Gradishar WJ, Perez EA, Martino S, Citron ML, Norton L, Winer EP, Hudis CA, Carey LA, Bernard PS, Nielsen TO, Perou CM, Ellis MJ, Barry WT. PAM50 gene signatures and breast cancer prognosis with adjuvant anthracycline- and taxane-based chemotherapy: correlative analysis of C9741 (Alliance). Npj Breast Cancer 2:15023 (2016) <http://dx.doi.org/10.1038/npjbcancer.2015.23>

\[2\] Barrett T, Wilhite SE, Ledoux P, Evangelista C, Kim IF, Tomashevsky M, Marshall KA, Phillippy KH, Sherman PM, Holko M, Yefanov A, Lee H, Zhang N, Robertson CL, Serova N, Davis S, Soboleva A. NCBI GEO: archive for functional genomics data sets--update. Nucleic Acids Res. 2013 Jan;41(Database issue):D991-5.

\[3\] Bruce JP, Hui AB, Shi W, Perez-Ordonez B et al. Identification of a microRNA signature associated with risk of distant metastasis in nasopharyngeal carcinoma. Oncotarget 2015 Feb 28;6(6):4537-50. PMID: 25738365
