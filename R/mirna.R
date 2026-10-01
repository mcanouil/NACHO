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

#' Ligation quality control of each sample
#'
#' NACHO's own definitions, since Bruker publishes no threshold: the three
#' ligation positive controls must be in order, their log2 counts must fall
#' on a line (R² against their positions, which does not depend on the
#' concentrations as long as each is the same fold below the previous one),
#' and the largest ligation negative must stay below the detection limit.
#'
#' @noRd
ligation_metrics <- function(counts, probes, limits) {
  names <- probes[["Name"]]
  positive <- match(c("LIG_POS_A", "LIG_POS_B", "LIG_POS_C"), names)
  negative <- probes[["CodeClass"]] == "Ligation" & grepl("^LIG_NEG", names)
  metrics <- data.frame(row.names = seq_len(ncol(counts)))
  if (!anyNA(positive)) {
    positives <- counts[positive, , drop = FALSE]
    metrics[["Ligation_order"]] <- as.numeric(
      positives[1, ] > positives[2, ] & positives[2, ] > positives[3, ]
    )
    metrics[["Ligation_R2"]] <- unname(apply(
      log2(positives + 1),
      2,
      function(m) {
        if (length(unique(m)) == 1) 0 else stats::cor(m, 3:1)^2
      }
    ))
  }
  if (any(negative)) {
    metrics[["Ligation_NEG"]] <- unname(
      apply(counts[negative, , drop = FALSE], 2, function(v) {
        if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
      }) -
        limits
    )
  }
  metrics
}

#' Haemolysis of plasma and serum samples
#'
#' `log2(miR-451a + 1) - log2(miR-23a-3p + 1)`, the count analogue of the
#' qPCR delta Cq, where above 7 suggests haemolysis (Blondal et al. 2013,
#' Methods 59, S1).
#'
#' @noRd
haemolysis_metric <- function(counts, probes) {
  rows <- match(c("hsa-miR-451a", "hsa-miR-23a-3p"), probes[["Name"]])
  if (anyNA(rows)) {
    return(NULL)
  }
  unname(log2(counts[rows[1], ] + 1) - log2(counts[rows[2], ] + 1))
}
