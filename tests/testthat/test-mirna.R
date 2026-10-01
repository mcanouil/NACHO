test_that("miRNA panels are detected from ligation probes or the GeneRLF", {
  probes <- data.frame(
    CodeClass = c("Positive", "Ligation"),
    Name = c("POS_A(128)", "LIG_POS_A")
  )
  expect_identical(NACHO:::detect_panel(probes, data.frame(x = 1)), "mirna")
  expect_identical(
    NACHO:::detect_panel(
      probes[1, ],
      data.frame(Sample_Attributes.sample_GeneRLF = "NS_H_miR_v3b")
    ),
    "mirna"
  )
  expect_identical(
    NACHO:::detect_panel(
      probes[1, ],
      data.frame(Sample_Attributes.sample_GeneRLF = "NS_IO_360_v1.0")
    ),
    "mrna"
  )
})

test_that("miRNA panels skip housekeeping normalisation by default", {
  x <- mirna_fixture()
  expect_identical(x@settings$panel, "mirna")
  expect_false(x@settings$housekeeping_norm)
  expect_true(all(is.na(nacho_qc(x)$Housekeeping_detected_status)))
  y <- mirna_fixture(housekeeping_norm = TRUE)
  expect_true(y@settings$housekeeping_norm)
})

test_that("mRNA panels keep housekeeping normalisation by default", {
  expect_identical(GSE74821@settings$panel, "mrna")
  expect_true(GSE74821@settings$housekeeping_norm)
})

test_that("each miRNA method scales by its own reference probes", {
  x <- mirna_fixture()
  counts <- nacho_counts(x)
  probes <- nacho_probes(x)
  expect_setequal(
    NACHO:::mirna_reference("spike_in", counts, probes),
    probes$Name[probes$CodeClass == "SpikeIn"]
  )
  expect_setequal(
    NACHO:::mirna_reference("ligation", counts, probes),
    c("LIG_POS_A", "LIG_POS_B", "LIG_POS_C")
  )
  endogenous <- grepl("Endogenous", probes$CodeClass)
  expect_setequal(
    NACHO:::mirna_reference("total_mirna", counts, probes),
    probes$Name[endogenous & apply(counts > 50, 1, all)]
  )
  expect_length(NACHO:::mirna_reference("stable_mirna", counts, probes), 5)
})

test_that("miRNA methods set the content factor and record the probes", {
  for (method in c("stable_mirna", "total_mirna", "spike_in", "ligation")) {
    x <- mirna_fixture(normalisation_method = method)
    expect_true("House_factor" %in% names(nacho_samples(x)), info = method)
    expect_gt(length(x@provenance$content_probes), 0)
  }
})

test_that("miRNA methods are refused for mRNA panels", {
  expect_error(
    normalise(GSE74821, normalisation_method = "spike_in"),
    regexp = "miRNA",
    class = "nacho_error_bad_argument"
  )
})
