#' qc_rcc
#'
#' @param data_dir [character]
#' @param nacho_df [data.frame]
#' @param id_colname [character]
#' @param housekeeping_genes [vector(character)]
#' @param predict_housekeeping [logical]
#' @param normalisation_method [character]
#' @param n_comp [numeric]
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom dplyr filter full_join
#' @importFrom tibble rownames_to_column
qc_rcc <- function(
  data_dir,
  nacho_df,
  id_colname,
  housekeeping_genes,
  predict_housekeeping,
  normalisation_method,
  n_comp
) {
  if (is.null(housekeeping_genes)) {
    housekeeping_genes <- unique(dplyr::filter(
      .data = nacho_df,
      grepl("Housekeeping", CodeClass)
    )[["Name"]])
  }
  control_genes_df <- dplyr::filter(
    .data = nacho_df,
    Name%in%housekeeping_genes | !grepl("Endogenous", CodeClass)
  )
  control_genes_df <- format_counts(
    data = control_genes_df,
    id_colname = id_colname,
    count_column = "Count"
  )
  rownames(control_genes_df) <- control_genes_df[["Name"]]

  probes_to_exclude <- probe_exclusion(control_genes_df = control_genes_df)

  if (predict_housekeeping) {
    temp_facs <- factor_calculation(
      nacho_df = nacho_df,
      id_colname = id_colname,
      housekeeping_genes = housekeeping_genes,
      predict_housekeeping = is.null(housekeeping_genes),
      normalisation_method = normalisation_method,
      exclude_probes = probes_to_exclude
    )

    tmp_counts <- dplyr::full_join(x = nacho_df, y = temp_facs, by = id_colname)
    tmp_counts[, "count_norm"] <- normalise_counts(data = tmp_counts)

    predicted_housekeeping <- find_housekeeping(
      data = tmp_counts,
      id_colname = id_colname,
      count_column = "count_norm"
    )

    if (!is.null(predicted_housekeeping)) {
      housekeeping_genes <- predicted_housekeeping
      control_genes_df <- dplyr::filter(
        .data = nacho_df,
        Name%in%predicted_housekeeping
      )
      control_genes_df <- format_counts(
        data = control_genes_df,
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
    predict_housekeeping = is.null(housekeeping_genes),
    normalisation_method = normalisation_method,
    exclude_probes = probes_to_exclude
  )

  counts_df <- format_counts(
    data = nacho_df,
    id_colname = id_colname,
    count_column = "Count"
  )
  counts_df <- counts_df[, sapply(X = counts_df, FUN = is.numeric)]

  if (n_comp > (ncol(counts_df)-1)) {
    message(paste('"n_comp" has been set to "n-1:"', (ncol(counts_df)-1)))
    n_comp <- (ncol(counts_df)-1)
  }
  pcas <- qc_pca(counts = counts_df, n_comp = n_comp)

  pcsum <- t(as.matrix(pcas[["pcsum"]]))
  pcsum <- as.data.frame(pcsum)
  pcsum[, "PC"] <- sprintf("PC%02d", as.numeric(gsub("PC", "", rownames(pcsum))))

  facs_pc_qc <- dplyr::full_join(
    x = dplyr::full_join(
      x = qc_values,
      y = tibble::rownames_to_column(df = as.data.frame(pcas[["pc"]], stringsAsFactors = FALSE), var = id_colname),
      by = id_colname
    ),
    y = norm_factor,
    by = id_colname
  )


  nacho_out <- dplyr::full_join(
    x = nacho_df[, c(id_colname, setdiff(colnames(nacho_df), colnames(facs_pc_qc)))],
    y = facs_pc_qc,
    by = id_colname
  )

  qc_out <- list(
    "access" = id_colname,
    "housekeeping_genes" = housekeeping_genes,
    "normalisation_method" = normalisation_method,
    "remove_outliers" = FALSE,
    "n_comp" = n_comp,
    "data_directory" = data_dir,
    "pc_sum" = pcsum,
    "nacho" = as.data.frame(nacho_out, stringsAsFactors = FALSE)
  )
  return(qc_out)
}