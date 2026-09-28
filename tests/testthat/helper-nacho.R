toy_nacho <- function(n_samples = 4L) {
  ids <- sprintf("S%02d.RCC", seq_len(n_samples))
  probes <- data.frame(
    CodeClass = c(
      rep("Positive", 6),
      rep("Negative", 2),
      "Housekeeping",
      "Endogenous",
      "Endogenous"
    ),
    Name = c(
      sprintf("POS_%s(%s)", LETTERS[1:6], c(128, 32, 8, 2, 0.5, 0.125)),
      "NEG_A(0)",
      "NEG_B(0)",
      "HK1",
      "GENE1",
      "GENE2"
    ),
    Accession = sprintf("ACC%02d", 1:11),
    is_housekeeping = c(rep(FALSE, 8), TRUE, FALSE, FALSE),
    is_excluded = FALSE
  )
  counts <- matrix(
    seq_len(11L * n_samples) + 10L,
    nrow = 11,
    dimnames = list(probes$Name, ids)
  )
  samples <- data.frame(
    IDFILE = ids,
    CartridgeID = rep(c("C1", "C2"), length.out = n_samples),
    BD = 1,
    FoV = 100,
    PCL = 0.99,
    LoD = 5,
    MC = 10,
    MedC = 10,
    Positive_factor = 1,
    Negative_factor = 1,
    House_factor = 1,
    is_outlier = FALSE
  )
  thresholds <- NACHO:::default_thresholds()
  provenance <- NACHO:::new_provenance(
    data_directory = NULL,
    file_version = "1.7",
    software_version = "4.0.0.3"
  )
  provenance[["nacho_version"]] <- "0.0.0"
  NACHO:::nacho(
    counts = counts,
    normalised = counts * 1,
    probes = probes,
    samples = samples,
    settings = list(
      id_colname = "IDFILE",
      housekeeping_genes = "HK1",
      housekeeping_predict = FALSE,
      housekeeping_norm = TRUE,
      normalisation_method = "GEO",
      n_comp = 2L
    ),
    thresholds = thresholds,
    pca = suppressWarnings(NACHO:::compute_pca(counts, 2L)),
    rcc_type = "n1",
    provenance = provenance
  )
}
