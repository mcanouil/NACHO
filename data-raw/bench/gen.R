# Writes n synthetic single-sample RCC files (774 probes) and a sample sheet.
generate_rcc <- function(
  dir,
  n,
  n_endogenous = 750,
  n_housekeeping = 10,
  seed = 1
) {
  set.seed(seed)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  concentration <- c(A = 128, B = 32, C = 8, D = 2, E = 0.5, F = 0.125)
  genes <- sprintf("GENE%04d", seq_len(n_endogenous))
  housekeeping <- sprintf("HK%02d", seq_len(n_housekeeping))
  baseline <- stats::rlnorm(n_endogenous, 5, 2)
  housekeeping_baseline <- stats::rlnorm(n_housekeeping, 8, 0.5)
  for (i in seq_len(n)) {
    size_factor <- stats::rlnorm(1, 0, 0.3)
    cartridge <- (i - 1) %/% 12 + 1
    code_summary <- c(
      sprintf(
        "Positive,POS_%s(%s),ERCC_%05d.1,%d",
        names(concentration),
        concentration,
        seq_along(concentration),
        stats::rpois(6, concentration * 400 * size_factor)
      ),
      sprintf(
        "Negative,NEG_%s(0),ERCC_%05d.1,%d",
        LETTERS[1:8],
        100 + 1:8,
        stats::rpois(8, 10)
      ),
      sprintf(
        "Housekeeping,%s,NM_%06d.1,%d",
        housekeeping,
        seq_along(housekeeping),
        stats::rpois(n_housekeeping, housekeeping_baseline * size_factor)
      ),
      sprintf(
        "Endogenous,%s,NM_%06d.1,%d",
        genes,
        1000 + seq_along(genes),
        stats::rpois(n_endogenous, baseline * size_factor) +
          stats::rpois(n_endogenous, 10)
      )
    )
    writeLines(
      c(
        "<Header>",
        "FileVersion,1.7",
        "SoftwareVersion,4.0.0.3",
        "</Header>",
        "",
        "<Sample_Attributes>",
        sprintf("ID,S%04d", i),
        "Owner,",
        "Comments,",
        "Date,20200101",
        "GeneRLF,Synthetic",
        "SystemAPF,n6_vDV1",
        "</Sample_Attributes>",
        "",
        "<Lane_Attributes>",
        sprintf("ID,%d", (i - 1) %% 12 + 1),
        "FovCount,555",
        sprintf("FovCounted,%d", sample(400:555, 1)),
        "ScannerID,SCAN1",
        sprintf("StagePosition,%d", cartridge %% 4 + 1),
        sprintf("BindingDensity,%.2f", stats::runif(1, 0.05, 2.5)),
        sprintf("CartridgeID,CART%03d", cartridge),
        "CartridgeBarcode,1",
        "</Lane_Attributes>",
        "",
        "<Code_Summary>",
        "CodeClass,Name,Accession,Count",
        code_summary,
        "</Code_Summary>",
        "",
        "<Messages>",
        "</Messages>"
      ),
      file.path(dir, sprintf("S%04d.RCC", i))
    )
  }
  utils::write.csv(
    data.frame(
      IDFILE = sprintf("S%04d.RCC", seq_len(n)),
      group = rep(c("a", "b"), length.out = n)
    ),
    file.path(dir, "samplesheet.csv"),
    row.names = FALSE
  )
  invisible(dir)
}
