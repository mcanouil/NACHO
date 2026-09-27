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
  nacho_df,
  id_colname,
  housekeeping_genes,
  housekeeping_predict,
  housekeeping_norm,
  normalisation_method,
  n_comp
) {
  Name <- CodeClass <- NULL # no visible binding for global variable
  has_hkg <- grepl("Housekeeping", nacho_df[["CodeClass"]])
  if (is.null(housekeeping_genes) && any(has_hkg)) {
    housekeeping_genes <- nacho_df[["Name"]][has_hkg]
    housekeeping_genes <- unique(housekeeping_genes)
  }

  control_genes_df <- format_counts(
    data = nacho_df[
      Name %in% housekeeping_genes | !grepl("Endogenous", CodeClass)
    ],
    id_colname = id_colname,
    count_column = "Count"
  )

  probes_to_exclude <- probe_exclusion(control_genes_df = control_genes_df)

  if (housekeeping_predict) {
    nacho_inform("Searching for the best housekeeping genes.")
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
        .SDcols = c(
          id_colname,
          setdiff(colnames(nacho_df), colnames(temp_facs))
        )
      ],
      y = temp_facs,
      by = id_colname,
      all = TRUE
    )
    tmp_counts[["count_norm"]] <- normalise_counts(
      data = tmp_counts,
      housekeeping_norm = FALSE
    )

    predicted_housekeeping <- find_housekeeping(
      data = data.table::setDT(tmp_counts),
      id_colname = id_colname,
      count_column = "count_norm"
    )

    if (
      is.null(predicted_housekeeping) || length(predicted_housekeeping) == 0
    ) {
      nacho_warn(
        "No suitable housekeeping genes were found; the default ones are used.",
        class = "no_housekeeping"
      )
    } else {
      nacho_inform(c(
        "Normalising with the predicted housekeeping genes:",
        stats::setNames(
          predicted_housekeeping,
          rep("*", length(predicted_housekeeping))
        )
      ))
      housekeeping_genes <- predicted_housekeeping

      control_genes_df <- format_counts(
        data = nacho_df[Name %in% housekeeping_genes],
        id_colname = id_colname,
        count_column = "Count"
      )
      rownames(control_genes_df) <- control_genes_df[["Name"]]
    }
  }

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
  counts_matrix <- as.matrix(counts_df[j = .SD, .SDcols = is.numeric])
  pca <- compute_pca(counts_matrix, n_comp)

  facs_pc_qc <- merge(
    x = qc_values,
    y = norm_factor,
    by = id_colname,
    all = TRUE
  )

  nacho_out <- merge(
    x = nacho_df[
      j = .SD,
      .SDcols = c(
        id_colname,
        setdiff(colnames(nacho_df), colnames(facs_pc_qc))
      )
    ],
    y = facs_pc_qc,
    by = id_colname,
    all = TRUE
  )

  list(
    housekeeping_genes = housekeeping_genes,
    pca = pca,
    nacho = nacho_out
  )
}
