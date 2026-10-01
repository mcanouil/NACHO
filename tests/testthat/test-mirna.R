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
