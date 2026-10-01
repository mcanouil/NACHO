#' @include ruv.R
NULL

#' Tell miRNA panels from mRNA panels
#'
#' @param probes The probe table, with a `CodeClass` column.
#' @param samples The sample table, which may hold the GeneRLF attribute.
#'
#' @return `"mirna"` when there are `Ligation` probes or the GeneRLF names a
#'   miRNA panel, and `"mrna"` otherwise.
#'
#' @noRd
detect_panel <- function(probes, samples) {
  gene_rlf <- samples[["Sample_Attributes.sample_GeneRLF"]]
  if (any(probes[["CodeClass"]] == "Ligation") || any(grepl("miR", gene_rlf))) {
    "mirna"
  } else {
    "mrna"
  }
}
