geometric_mean_ref <- function(x) {
  x[x == 0] <- 1
  exp(mean(log(x)))
}

per_sample_ref <- function(nacho_object, columns) {
  nacho_df <- data.table::as.data.table(nacho_object[["nacho"]])
  id <- nacho_object[["access"]]
  out <- unique(nacho_df[, c(id, columns), with = FALSE])
  out[order(out[[id]])]
}

by_sample_ref <- function(data, id, fun) {
  as.vector(tapply(data[["Count"]], data[[id]], fun))
}

gse_geo <- suppressMessages(normalise(GSE74821, normalisation_method = "GEO"))
gse_df <- data.table::as.data.table(gse_geo[["nacho"]])
gse_id <- gse_geo[["access"]]
gse_samples <- per_sample_ref(
  gse_geo,
  c("Positive_factor", "Negative_factor", "House_factor", "PCL", "LoD", "FoV")
)
