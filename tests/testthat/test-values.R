test_that("positive factor is the mean over the sample geometric means of POS_A to POS_E", {
  positives <- gse_df[
    gse_df[["CodeClass"]] == "Positive" & gse_df[["Name"]] != "POS_F(0.125)"
  ]
  geometric_means <- by_sample_ref(positives, gse_id, geometric_mean_ref)
  expect_equal(
    gse_samples[["Positive_factor"]],
    mean(geometric_means) / geometric_means
  )
})

test_that("negative factor is the geometric mean of the negatives kept after exclusion", {
  negatives <- gse_df[gse_df[["CodeClass"]] == "Negative"]
  wide <- data.table::dcast(
    negatives,
    stats::as.formula(paste("Name ~", gse_id)),
    value.var = "Count"
  )
  counts <- as.matrix(wide[, -1])
  probe_medians <- apply(counts, 1, stats::median)
  overall_median <- stats::median(counts)
  excluded <- wide[["Name"]][
    abs(overall_median - probe_medians) > 0.5 * overall_median
  ]
  kept <- negatives[!negatives[["Name"]] %in% excluded]
  expect_equal(
    gse_samples[["Negative_factor"]],
    by_sample_ref(kept, gse_id, geometric_mean_ref)
  )
})

test_that("housekeeping factor uses background-corrected counts floored at 1", {
  housekeeping <- merge(
    gse_df[
      gse_df[["Name"]] %in% gse_geo[["housekeeping_genes"]],
      c(gse_id, "Name", "Count"),
      with = FALSE
    ],
    gse_samples[,
      c(gse_id, "Positive_factor", "Negative_factor"),
      with = FALSE
    ],
    by = gse_id
  )
  housekeeping[["Count"]] <- pmax(
    (housekeeping[["Count"]] - housekeeping[["Negative_factor"]]) *
      housekeeping[["Positive_factor"]],
    1
  )
  geometric_means <- by_sample_ref(housekeeping, gse_id, geometric_mean_ref)
  expect_equal(
    gse_samples[["House_factor"]],
    mean(geometric_means) / geometric_means
  )
})

test_that("positive control linearity is the R-squared of log2 counts on log2 concentrations", {
  positives <- gse_df[gse_df[["CodeClass"]] == "Positive"]
  expected <- vapply(
    X = split(positives, positives[[gse_id]]),
    FUN = function(sample) {
      concentration <- as.numeric(sub(
        "^[^(]*\\((.*)\\)$",
        "\\1",
        sample[["Name"]]
      ))
      measured <- if (any(sample[["Count"]] == 0)) {
        log2(sample[["Count"]] + 1)
      } else {
        log2(sample[["Count"]])
      }
      fit <- summary(stats::lm(measured ~ log2(concentration)))
      round(fit[["r.squared"]], 5)
    },
    FUN.VALUE = numeric(1)
  )
  expect_equal(gse_samples[["PCL"]], unname(expected))
})

test_that("limit of detection is the z-score of POS_E against the negatives", {
  expected <- vapply(
    X = split(gse_df, gse_df[[gse_id]]),
    FUN = function(sample) {
      pos_e <- sample[sample[["Name"]] == "POS_E(0.5)"][["Count"]]
      negatives <- sample[sample[["CodeClass"]] == "Negative"][["Count"]]
      round((pos_e - mean(negatives)) / stats::sd(negatives), 2)
    },
    FUN.VALUE = numeric(1)
  )
  expect_equal(gse_samples[["LoD"]], unname(expected))
})

test_that("field of view is the percentage of counted fields", {
  lanes <- per_sample_ref(
    gse_geo,
    c("Lane_Attributes.lane_FovCounted", "Lane_Attributes.lane_FovCount")
  )
  expected <- round(
    as.numeric(lanes[["Lane_Attributes.lane_FovCounted"]]) /
      as.numeric(lanes[["Lane_Attributes.lane_FovCount"]]) *
      100,
    2
  )
  expect_equal(gse_samples[["FoV"]], expected)
})

test_that("normalised counts above background are corrected, scaled and rounded", {
  expected <- (gse_df[["Count"]] - gse_df[["Negative_factor"]]) *
    gse_df[["Positive_factor"]] *
    gse_df[["House_factor"]]
  above_background <- expected >= 1
  expect_equal(
    gse_df[["Count_Norm"]][above_background],
    round(expected[above_background])
  )
})

test_that("PCA stores sample scores on log counts", {
  wide <- data.table::dcast(
    gse_df,
    stats::as.formula(paste("CodeClass + Name ~", gse_id)),
    value.var = "Count"
  )
  counts <- as.matrix(wide[, -c(1, 2)])
  expected <- stats::prcomp(t(log(counts + 1)))
  pcs <- per_sample_ref(
    gse_geo,
    sprintf("PC%02d", seq_len(gse_geo[["n_comp"]]))
  )
  expect_equal(
    abs(unname(as.matrix(pcs[, -1]))),
    abs(unname(expected[["x"]][pcs[[gse_id]], seq_len(gse_geo[["n_comp"]])]))
  )
  expect_equal(
    gse_geo[["pc_sum"]][["Proportion of Variance"]],
    unname(summary(expected)[["importance"]][
      "Proportion of Variance",
      seq_len(gse_geo[["n_comp"]])
    ])
  )
})
