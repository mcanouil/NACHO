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

#' miRNA content normalisation methods
#'
#' @noRd
mirna_methods <- c("stable_mirna", "total_mirna", "spike_in", "ligation")

#' Reference probes of a miRNA content normalisation
#'
#' The order follows Bruker's technical note on plasma and serum miRNA:
#' stable miRNAs (the five most stable by geNorm among miRNAs above
#' background in 90 % of samples), total miRNA (miRNAs above 50 counts in
#' every sample), spike-ins, then ligation positive controls.
#'
#' @noRd
mirna_reference <- function(
  method,
  counts,
  probes,
  call = rlang::caller_env()
) {
  code_class <- probes[["CodeClass"]]
  names <- probes[["Name"]]
  endogenous <- grepl("Endogenous", code_class)
  reference <- switch(
    method,
    stable_mirna = predict_housekeeping(
      counts[endogenous, , drop = FALSE],
      probes[endogenous, , drop = FALSE]
    ),
    total_mirna = names[
      endogenous & rowSums(!(counts > 50) | is.na(counts)) == 0
    ],
    spike_in = names[code_class == "SpikeIn"],
    ligation = names[code_class == "Ligation" & grepl("^LIG_POS", names)]
  )
  if (length(reference) == 0) {
    nacho_abort(
      "{.code normalisation_method = {.val {method}}} found no reference probes in these data.",
      class = "bad_argument",
      call = call
    )
  }
  reference
}
