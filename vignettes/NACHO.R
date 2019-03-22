## ----ex1, eval=FALSE-----------------------------------------------------
#  library(NACHO)
#  data(GSE74821)
#  visualise(GSE74821)

## ----ex2, results = "hide", message = FALSE, warning = FALSE-------------
library(GEOquery)
#Download data
gse <- getGEO("GSE70970")
#Get phenotypes
targets <- pData(phenoData(gse[[1]]))
getGEOSuppFiles("GSE70970")
#Unzip data
untar("GSE70970/GSE70970_RAW.tar", exdir = "GSE70970/Data")
#Add IDs
IDs <- list.files("GSE70970/Data")
targets$IDFILE <- IDs

## ----ex3-----------------------------------------------------------------
library(NACHO)
GSE70970_sum <- summarize(data_directory =  "GSE70970/Data/", # Where the data is
                 ssheet_csv = targets, # The samplesheet
                 normalisation_method="GEO", # Geometric mean or GLM
                 housekeeping_genes = NULL, # Custom list of housekeeping genes
                 id_colname = "IDFILE", # Name of the column that contains the identfiers
                 housekeeping_predict = TRUE, # Predict the housekeeping genes based on the data?
                 n_comp=5) # Number indicating the number of principal components to compute. 

## ---- eval=FALSE---------------------------------------------------------
#  visualise(GSE70970_sum)

## ----ex5-----------------------------------------------------------------
print(GSE70970_sum[["housekeeping_genes"]])

## ----ex6-----------------------------------------------------------------
my_housekeeping <- GSE70970_sum[["housekeeping_genes"]][-c(1, 2)]
print(my_housekeeping)

## ----ex7-----------------------------------------------------------------
GSE70970_norm <- normalise(
  nacho_object = GSE70970_sum,
  housekeeping_genes = my_housekeeping,
  housekeeping_norm = TRUE,
  normalisation_method = "GEO", 
  remove_outliers = TRUE
)

