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

test_that("ligation metrics follow the NACHO definitions", {
  counts <- matrix(
    c(12807, 1715, 293, 9, 11, 8, 13, 6, 8, 13, 4, 3, 12, 14),
    ncol = 1,
    dimnames = list(
      c(
        "LIG_POS_A",
        "LIG_POS_B",
        "LIG_POS_C",
        "LIG_NEG_A",
        "LIG_NEG_B",
        "LIG_NEG_C",
        sprintf("NEG_%s(0)", LETTERS[1:8])
      ),
      "S1"
    )
  )
  probes <- data.frame(
    CodeClass = c(rep("Ligation", 6), rep("Negative", 8)),
    Name = rownames(counts)
  )
  negatives <- counts[7:14, 1]
  limit <- mean(negatives) + 2 * stats::sd(negatives)
  out <- NACHO:::ligation_metrics(counts, probes, limit)
  expect_identical(out$Ligation_order, 1)
  expect_equal(
    out$Ligation_R2,
    stats::cor(log2(c(12807, 1715, 293) + 1), 3:1)^2
  )
  expect_equal(out$Ligation_NEG, 11 - limit)
  expect_identical(
    ncol(NACHO:::ligation_metrics(
      counts[7:14, , drop = FALSE],
      probes[7:14, ],
      limit
    )),
    0L
  )
})

test_that("haemolysis is the log2 ratio of miR-451a to miR-23a-3p", {
  x <- mirna_fixture()
  counts <- nacho_counts(x)
  expect_equal(
    nacho_samples(x)$Haemolysis,
    unname(
      log2(counts["hsa-miR-451a", ] + 1) - log2(counts["hsa-miR-23a-3p", ] + 1)
    )
  )
  expect_true(all(nacho_qc(x)$Haemolysis_status == "pass"))
  x@thresholds <- nacho_thresholds("sprint", haemolysis = TRUE)
  expect_identical(
    nacho_qc(x)$Haemolysis_status == "fail",
    nacho_samples(x)$Haemolysis > 7
  )
  expect_null(NACHO:::haemolysis_metric(
    counts[1:3, ],
    data.frame(Name = rownames(counts)[1:3])
  ))
})

test_that("GSE270837 passes ligation QC and legacy never flags it", {
  x <- mirna_fixture()
  qc <- nacho_qc(x)
  expect_true(all(qc$Ligation_order_status == "pass"))
  expect_true(all(qc$Ligation_R2_status == "pass"))
  expect_true(all(qc$Ligation_NEG_status == "pass"))
  x@samples$Ligation_R2 <- 0.1
  x@thresholds <- nacho_thresholds("sprint", preset = "legacy")
  expect_true(all(nacho_qc(x)$Ligation_R2_status == "pass"))
})

test_that("mRNA panels get no ligation or haemolysis columns", {
  x <- NACHO::GSE74821
  expect_false(any(
    c("Ligation_order", "Ligation_R2", "Ligation_NEG", "Haemolysis") %in%
      names(nacho_samples(x))
  ))
  expect_false(any(grepl("^(Ligation|Haemolysis)", names(nacho_qc(x)))))
})

test_that("a constant ligation series gives an R2 of 0 without a warning", {
  counts <- rbind(
    LIG_POS_A = c(0, 100),
    LIG_POS_B = c(0, 10),
    LIG_POS_C = c(0, 1),
    LIG_NEG_A = c(5, 5),
    LIG_NEG_B = c(5, 5)
  )
  probes <- data.frame(
    CodeClass = c(rep("Ligation", 5)),
    Name = rownames(counts)
  )
  expect_no_warning(out <- NACHO:::ligation_metrics(counts, probes, c(1, 1)))
  expect_identical(out$Ligation_R2[1], 0)
  expect_gt(out$Ligation_R2[2], 0.95)
})

test_that("the nSolver preset flags failed ligation", {
  x <- mirna_fixture()
  x@samples$Ligation_order[1] <- 0
  x@samples$Ligation_R2[2] <- 0.1
  x@samples$Ligation_NEG[3] <- 1
  qc <- nacho_qc(x)
  expect_identical(qc$Ligation_order_status[1], "fail")
  expect_identical(qc$Ligation_R2_status[2], "fail")
  expect_identical(qc$Ligation_NEG_status[3], "fail")
})

test_that("miRNA panels without housekeeping normalisation have no House_factor", {
  x <- mirna_fixture()
  expect_false("House_factor" %in% names(nacho_samples(x)))
  expect_false("House_factor_status" %in% names(nacho_qc(x)))
  y <- mirna_fixture(housekeeping_norm = TRUE)
  expect_true("House_factor" %in% names(nacho_samples(y)))
})

test_that("every miRNA method is a normalisation method", {
  expect_true(all(
    NACHO:::mirna_methods %in% NACHO:::nacho_normalisation_methods
  ))
})

test_that("summary() copes with thresholds that lack the miRNA metrics", {
  x <- mirna_fixture()
  thresholds <- x@thresholds
  thresholds[["Ligation_order"]] <- NULL
  x@thresholds <- thresholds
  expect_false("Ligation_order" %in% summary(x)$metric)
})

test_that("the report lists only the thresholds of metrics the data have", {
  output <- capture.output(NACHO:::report_markdown(GSE74821))
  expect_false(any(grepl("Ligation", output)))
})

test_that("a partial ligation control set keeps the metrics it can compute", {
  probes <- data.frame(
    CodeClass = c("Ligation", "Ligation", "Ligation", "Ligation"),
    Name = c("LIG_POS_A", "LIG_POS_B", "LIG_POS_C", "LIG_NEG_A")
  )
  counts <- matrix(c(800, 200, 50, 5, 700, 180, 40, 1), nrow = 4)
  both <- NACHO:::ligation_metrics(counts, probes, c(2, 2))
  expect_named(both, c("Ligation_order", "Ligation_R2", "Ligation_NEG"))
  expect_identical(both$Ligation_NEG, c(3, -1))
  positives_only <- NACHO:::ligation_metrics(
    counts[1:3, ],
    probes[1:3, ],
    c(2, 2)
  )
  expect_named(positives_only, c("Ligation_order", "Ligation_R2"))
  negatives_only <- NACHO:::ligation_metrics(
    counts[c(1, 4), ],
    probes[c(1, 4), ],
    c(2, 2)
  )
  expect_named(negatives_only, "Ligation_NEG")
})
