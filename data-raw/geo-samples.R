# Sample table of a GEO series matrix, as a data frame with the GEO column names.
# GEOquery 2.82.0 and later return a SummarizedExperiment from getGEO().
geo_samples <- function(series_matrix) {
  if (methods::is(series_matrix, "SummarizedExperiment")) {
    as.data.frame(SummarizedExperiment::colData(series_matrix), optional = TRUE)
  } else {
    Biobase::pData(series_matrix)
  }
}
