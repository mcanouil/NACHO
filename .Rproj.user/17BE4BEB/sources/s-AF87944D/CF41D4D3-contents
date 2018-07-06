#' @title Summarize
#' @description Analyzes, calculates and summarizes nanoString nCounter RCC
#' files. The RCC files can be visually inspected through the summary when given as
#' input to the visualize function. 
#' @param ssheet Path to a samplesheet containing at least a column with the
#' exact names of the RCC files which ought to be analyzed / summarized.
#' @param id_colname Refers to the name of the column containing exact names
#' of the RCC files which ought to be analyzed / summarized.
#' @param housekeep Vector with the names of the housekeeping genes. When no input is given, 
#' the default housekeeping genes are used. When housekeep="predict" is used, the data will be 
#' analyzed and the 5 best genes which could serve as housekeeping genes are used.
#' @param norm A string indicating which form of calculation needs to be performed:
#' "GEO" or "GLM"
#' @return A list containing the QC metrics,the original samplesheet,principal components,
#' normalization factors, control genes and counts.
#' @references 
#' @examples
#' summarize("path/to/samplesheet.csv","FILE_COL","predict","GEO")
#' summarize("path/to/samplesheet.csv","FILE_COL","","GLM")
#' summarize("path/to/samplesheet.csv","FILE_COL,c("hsa-miR-361-5p","hsa-miR-186-5p","hsa-miR-26a-5p"),"GLM")
#' @export
summarize <- function(data_dir = NULL, ssheet=NULL,id_colname=NULL, housekeep="", norm="GEO"){
  require(reshape2)
  locations <- list.files(data_dir, pattern=".RCC",recursive = T)
  housekeep <- paste(housekeep,collapse = "@")
  ssheet_df <- load.samplesheet(ssheet)
  accession <- ssheet_df[,id_colname]
  location_match <- sapply(accession, function(y) locations[grep(y,locations)])
  names(location_match) <- accession
  location_match <- Filter(length,location_match)
  accession <- names(location_match)
  if(length(locations) > length(accession)){
    warning("djdjdjd")
  }
  path_to_files <- paste(data_dir,location_match[accession],sep="/")
  rcc_content <- lapply(path_to_files, rcc.read)
  counts <- lapply(rcc_content, extract_counts)
  names(counts) <- accession
  counts <- do.call(cbind,counts)
  control_genes <- lapply(rcc_content, control_genes, housekeep)
  names(control_genes) <- accession
  control_genes <- melt(control_genes,
                        id.vars=c("CodeClass","Name","Accession","Count"))
  exc_probes <- probe.exclusion(control_genes)
  if(housekeep == "predict"){
    temp_facs <- factor_calculation(rcc_content,housekeep,norm, exc_probes)
    rownames(temp_facs) <- accession
    tmp_counts <- lapply(colnames(counts), function(x) {
      local <- counts[,x] - temp_facs[x,"Negative_factor"]
      local <- counts[,x] * temp_facs[x,"Positive_factor"]
      local[local <= 0] <- 0.1
      local <- round(local)
      local
    })
    names(tmp_counts) <- accession
    tmp_counts<- do.call(cbind,tmp_counts)
    predicted_housekeeping <<- predict.housekeeping(tmp_counts)
    housekeep <- paste(predicted_housekeeping,collapse = "@")
    control_genes <- lapply(rcc_content, control_genes, housekeep)
    names(control_genes) <- accession
    control_genes <- melt(control_genes,
                          id.vars=c("CodeClass","Name","Accession","Count"))
  }
  qc_values <- lapply(rcc_content, qc_features)
  qc_values <- do.call(rbind, qc_values)
  qc_values <- as.data.frame(qc_values, stringsAsFactors=F)
  norm_factor <- factor_calculation(rcc_content,housekeep,norm, exc_probes)
  rownames(norm_factor) <- accession
  pcas <- prinicipal_components(counts)
  rownames(qc_values) <- accession
  rownames(ssheet_df) <- accession
  summar <- list("features" = qc_values,
                 "ssheet" = ssheet_df,
                 "pc" = pcas[[1]],
                 "pcsum" = pcas[[2]],
                 "norm_facs" = norm_factor,
                 "control" = control_genes,
                 "counts" = counts,
                 "path" = data_dir,
                 "access" = id_colname,
                 "locations" = location_match,
                 "housekeep" = housekeep)
  return(summar)
}

#' @title Normalize
#' @description Calculates sample specific size factors and normalizes
#' nanoString nCounter RCC files.
#' @param summary Summary of analyzed nanoString nCounter RCC files.
#' @param housekeep Vector with the names of the housekeeping genes. When no input is given, 
#' the default housekeeping genes are used.
#' @param remove.outliers Logical indicator if outliers ought to be removed before size factor
#' calculation and normalization. If outliers are not removed, the the calculated size factors 
#' from summarize will be used.
#' @param norm A string indicating which form of calculation needs to be performed:
#' "GEO" or "GLM"
#' @return A list containing the raw counts as data frame, normalized counts as data frame and
#' normalization factors as dataframe.
#' @examples
#' normalize(my.summary,predicted_housekeeping,T,"GLM)
#' normalize(my.summary,c("hsa-miR-361-5p","hsa-miR-186-5p","hsa-miR-26a-5p"),F,"GEO")
#' @export
normalize <- function(summary, housekeep="", remove.outliers=T, norm="GEO"){
  if(remove.outliers){
    housekeep <- paste(housekeep,collapse = "@")
    ssheet_df <- remove.outliers(summary)
    accession <- ssheet_df[,summary[["access"]]]
    path <- summary[["path"]]
    location_match <- summary[["locations"]]
    path_to_files <- paste(path,location_match[accession],sep="/")
    rcc_content <- lapply(path_to_files, rcc.read)
    control_genes <- lapply(rcc_content, control_genes, housekeep)
    names(control_genes) <- accession
    control_genes <- melt(control_genes,
                          id.vars=c("CodeClass","Name","Accession","Count"))
    exc_probes <- probe.exclusion(control_genes)
    norm_factor <- factor_calculation(rcc_content, housekeep = housekeep, norm, exc_probes)
    rownames(norm_factor) <- accession
    counts <- lapply(rcc_content, extract_counts)
    counts <- do.call(cbind,counts)
    colnames(counts) <- accession
    raw_counts <- counts
  }else{
    norm_factor <- summary[["norm_facs"]]
    counts <- summary[["counts"]]
  }
  counts <- sapply(colnames(counts), function(x) {
    local <- counts[,x] - norm_factor[x,"Negative_factor"]
    local
  })
  counts <- lapply(colnames(counts), function(x){
    local <- counts[,x] * norm_factor[x,"Positive_factor"] * norm_factor[x,"House_factor"]
    local[local <= 0] <- 0.1
    local <- round(local)
    local
  })
  names(counts) <- accession
  counts <-do.call(cbind.data.frame, counts)
  output <- list("counts" = raw_counts,
                 "scaling" = norm_factor,
                 "normalized" = counts)
  return(output)
}

#' @title Visualize
#' @description Visualizes the summarized nanoString nCounter RCC files in a 
#' Shiny dashboard
#' @param summary Summary of analyzed nanoString nCounter RCC files.
#' @export
#' @import shiny
#' @import DT
#' @import ggplot2
#' @import ggbeeswarm
#' @import gtools
#' @import reshape2
visualize <- function(summary=NULL){
  require(shiny)
  require(DT)
  require(ggplot2)
  require(ggbeeswarm)
  require(gtools)
  require(reshape2)
  require(Biobase)
  if(is.null(summary)){
    stop("No data provided.")
  }
  if(T %in% is.na(match(names(summary), c("features",
                                                "ssheet",
                                                "pc",
                                                "pcsum",
                                                "norm_facs",
                                                "control",
                                                "counts",
                                                "path",
                                                "access","locations","housekeep")))){
    stop("No valid data provided. \n Use summarize() to generate data")
  }
  dir <- "./dashboard"
  #dir <- system.file("shiny-examples", "nacho_app", package = "testnacho")
  qcdata_ssheet <<- summary
  runApp(dir)
  return(getwd())
}


