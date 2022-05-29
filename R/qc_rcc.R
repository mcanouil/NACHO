#' qc_rcc
#'
#' @inheritParams load_rcc
#' @param nacho_df [[data.frame]] A `data.frame` with all columns from the sample sheet `ssheet_csv`
#'   and all computed columns, *i.e.*, quality-control metrics and counts, with one sample per row.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[list]]
qc_rcc <- function(
  data_directory,
  nacho_df,
  id_colname,
  housekeeping_genes,
  housekeeping_predict,
  housekeeping_norm,
  normalisation_method,
  n_comp
) {
  has_hkg <- grepl("Housekeeping", nacho_df[["CodeClass"]])
  if (is.null(housekeeping_genes) & any(has_hkg)) {
    housekeeping_genes <- nacho_df[["Name"]][has_hkg]
    housekeeping_genes <- unique(housekeeping_genes)
  }
  if (!inherits(nacho_df, "data.table")) {
    nacho_df <- data.table::as.data.table(nacho_df)
  }
  control_genes_df <- nacho_df[Name %in% housekeeping_genes | !grepl("Endogenous", CodeClass)]

  control_genes_df <- format_counts(
    data = control_genes_df,
    id_colname = id_colname,
    count_column = "Count"
  )

  probes_to_exclude <- probe_exclusion(control_genes_df = control_genes_df)

  if (housekeeping_predict) {
    message("[NACHO] Searching for the best housekeeping genes.")
    message(
      '[NACHO] Computing normalisation factors using "',
      normalisation_method,
      '" method for housekeeping genes prediction.'
    )
    temp_facs <- factor_calculation(
      nacho_df = nacho_df,
      id_colname = id_colname,
      housekeeping_genes = housekeeping_genes,
      housekeeping_predict = housekeeping_predict,
      normalisation_method = normalisation_method,
      exclude_probes = probes_to_exclude
    )

    tmp_counts <- merge(
      x = nacho_df[
        j = .SD,
        .SDcols = c(id_colname, setdiff(colnames(nacho_df), colnames(temp_facs)))
      ],
      y = temp_facs,
      by = id_colname,
      all = TRUE
    )
    tmp_counts[["count_norm"]] <- normalise_counts(data = tmp_counts, housekeeping_norm = FALSE)

    predicted_housekeeping <- find_housekeeping(
      data = tmp_counts,
      id_colname = id_colname,
      count_column = "count_norm"
    )

    if (is.null(predicted_housekeeping) | length(predicted_housekeeping) == 0) {
      message("[NACHO] Could not find suitable houskeeping genes, default will be used.")
    } else {
      message(
        "[NACHO] The following predicted housekeeping genes will be used for normalisation:\n",
          paste0("  - ", predicted_housekeeping, collapse = "\n")
      )
      housekeeping_genes <- predicted_housekeeping
      control_genes_df <- nacho_df[nacho_df[["Name"]] %in% predicted_housekeeping, ]
      control_genes_df <- format_counts(
        data = control_genes_df,
        id_colname = id_colname,
        count_column = "Count"
      )
      rownames(control_genes_df) <- control_genes_df[["Name"]]
    }
  }

  message('[NACHO] Computing normalisation factors using "', normalisation_method, '" method.')
  qc_values <- qc_features(data = nacho_df, id_colname = id_colname)
  norm_factor <- factor_calculation(
    nacho_df = nacho_df,
    id_colname = id_colname,
    housekeeping_genes = housekeeping_genes,
    housekeeping_predict = FALSE,
    normalisation_method = normalisation_method,
    exclude_probes = probes_to_exclude
  )

  counts_df <- format_counts(
    data = nacho_df,
    id_colname = id_colname,
    count_column = "Count"
  )
  counts_df <- counts_df[j = .SD, .SDcols = is.numeric]

  if (n_comp > (ncol(counts_df) - 1)) {
    message(paste('"n_comp" has been set to "n-1:"', (ncol(counts_df) - 1)))
    n_comp <- (ncol(counts_df) - 1)
  }

  if (anyNA(counts_df)) {
    message("[NACHO] Missing values have been replaced with zeros for PCA.")
    counts_df_tmp <- as.matrix(counts_df)
    counts_df_tmp[is.na(counts_df_tmp)] <- 0
  } else {
    counts_df_tmp <- counts_df
  }

  pcas <- qc_pca(counts = counts_df_tmp, n_comp = n_comp)

  pcsum <- as.data.frame(t(pcas[["pcsum"]]), stringsAsFactors = FALSE)
  rownames(pcsum) <- pcsum[["PC"]] <- sprintf("PC%02d", as.numeric(gsub("PC", "", rownames(pcsum))))

  pcas_pc <- as.data.frame(pcas[["pc"]], stringsAsFactors = FALSE)
  colnames(pcas_pc) <- sprintf("PC%02d", as.numeric(gsub("PC", "", colnames(pcas_pc))))
  pcas_pc[[id_colname]] <- rownames(pcas_pc)

  facs_pc_qc <- merge(
    x = merge(
      x = qc_values,
      y = pcas_pc,
      by = id_colname,
      all = TRUE
    ),
    y = norm_factor,
    by = id_colname,
    all = TRUE
  )

  nacho_out <- merge(
    x = nacho_df[
      j = .SD,
      .SDcols = c(id_colname, setdiff(colnames(nacho_df), colnames(facs_pc_qc)))
    ],
    y = facs_pc_qc,
    by = id_colname,
    all = TRUE
  )

  list(
    access = id_colname,
    housekeeping_genes = housekeeping_genes,
    housekeeping_predict = housekeeping_predict,
    housekeeping_norm = housekeeping_norm,
    normalisation_method = normalisation_method,
    remove_outliers = FALSE,
    n_comp = n_comp,
    data_directory = data_directory,
    pc_sum = pcsum,
    nacho = as.data.frame(nacho_out, stringsAsFactors = FALSE)
  )
}
