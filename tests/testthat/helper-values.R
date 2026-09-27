geometric_mean_ref <- function(x) {
  x[x == 0] <- 1
  exp(mean(log(x)))
}

per_sample_ref <- function(nacho_object, columns) {
  samples <- data.table::as.data.table(nacho_samples(nacho_object))
  out <- samples[, c("IDFILE", columns), with = FALSE]
  out[order(out[["IDFILE"]])]
}

by_sample_ref <- function(data, id, fun) {
  as.vector(tapply(data[["Count"]], data[[id]], fun))
}
