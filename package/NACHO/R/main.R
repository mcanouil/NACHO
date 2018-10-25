#' summarise
#'
#' @param data_directory [character]
#' @param ssheet_csv [character]
#' @param id_colname [character]
#' @param housekeeping_genes [vector(character)]
#' @param predict_housekeeping [logical]
#' @param normalisation_method [character]
#' @param n_comp [numeric]
#'
#' @return [nacho]
#' @export
#'
#' @examples
#' @importFrom purrr map_lgl map
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest unite
#' @importFrom utils read.csv
summarise <- function(
  data_directory = NULL,
  ssheet_csv = NULL,
  id_colname = NULL,
  housekeeping_genes = NULL,
  predict_housekeeping = FALSE,
  normalisation_method = "GEO",
  n_comp = 10
) {
  nacho_df <- utils::read.csv(file = ssheet_csv, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  nacho_df <- tibble::as_tibble(nacho_df)
  nacho_df[["file_path"]] <- paste(data_directory, nacho_df[[id_colname]], sep = "/")
  nacho_df[["file_exists"]] <- purrr::map_lgl(.x = nacho_df[["file_path"]], .f = file.exists)
  nacho_df[["rcc_content"]] <- purrr::map(.x = nacho_df[["file_path"]], .f = read_rcc)

  column_to_unnest <- c("rcc_content", "Code_Summary")
  nacho_df <- tidyr::unnest(data = nacho_df, rcc_content = get(column_to_unnest[1]), .drop = FALSE)
  nacho_df <- tidyr::unnest(data = nacho_df, Code_Summary = get(column_to_unnest[2]), .drop = FALSE)
  nacho_df[["CodeClass"]] <- gsub("Endogenous.*", "Endogenous", nacho_df[["CodeClass"]])

  if ("plexset_id" %in% colnames(nacho_df)) {
    nacho_df <- tidyr::unite(data = nacho_df, col = !!id_colname, id_colname, "plexset_id")
  }

  summary_out <- qc_rcc(
    data_dir = data_directory,
    nacho_df = nacho_df,
    id_colname = id_colname,
    housekeeping_genes = housekeeping_genes,
    predict_housekeeping = predict_housekeeping,
    normalisation_method = normalisation_method,
    n_comp = n_comp
  )

  return(summary_out)
}

#' normalise
#'
#' @param nacho_object [nacho]
#' @param housekeeping_genes [vector(character)]
#' @param normalisation_method [character]
#' @param remove_outliers [logical]
#'
#' @return [nacho]
#' @export
#'
#' @examples
normalise <- function(
  nacho_object,
  housekeeping_genes = nacho_object[["housekeeping_genes"]],
  normalisation_method = nacho_object[["normalisation_method"]],
  remove_outliers = TRUE
) {

  id_colname <- nacho_object[["access"]]

  if (!all.equal(sort(nacho_object[["housekeeping_genes"]]), sort(housekeeping_genes))) {
    warning(
      paste0(
        '"housekeeping_genes" is different from the parameter used to import RCC files!\n',
        '"summarise()" parameter:\n',
        '    housekeeping_genes=', deparse(nacho_object[["housekeeping_genes"]]), '\n',
        '"normalise()" parameter:\n',
        '    housekeeping_genes=', deparse(housekeeping_genes), '\n'
      )
    )
  }

  if (nacho_object[["normalisation_method"]]!=normalisation_method) {
    warning(
      paste0(
        '"normalisation_method" is different from the parameter used to import RCC files!\n',
        '"summarise()" parameter:\n',
        '    normalisation_method=', deparse(nacho_object[["normalisation_method"]]), '\n',
        '"normalise()" parameter:\n',
        '    normalisation_method=', deparse(normalisation_method), '\n'
      )
    )
  }

  if (nacho_object[["remove_outliers"]]) {
    message("Outliers have already been removed!")
  }

  if (remove_outliers & !nacho_object[["remove_outliers"]]) {
    nacho_df <- exclude_outliers(object = nacho_object)
    outliers <- setdiff(
      unique(nacho_object[["nacho"]][[nacho_object[["access"]]]]),
      unique(nacho_df[[nacho_object[["access"]]]])
    )
    if (length(outliers)!=0) {
      nacho_object <- qc_rcc(
        data_dir = nacho_object[["data_directory"]],
        nacho_df = nacho_df,
        id_colname = id_colname,
        housekeeping_genes = housekeeping_genes,
        predict_housekeeping = FALSE,
        normalisation_method = normalisation_method,
        n_comp = nacho_object[["n_comp"]]
      )
    }
    nacho_object[["remove_outliers"]] <- remove_outliers
  }

  nacho_object[["nacho"]][["Count_Norm"]] <- normalise_counts(data = nacho_object[["nacho"]])

  raw_counts <- format_counts(
    data = nacho_object[["nacho"]],
    id_colname = id_colname,
    count_column = "Count"
  )
  nacho_object[["raw_counts"]] <- raw_counts

  norm_counts <- format_counts(
    data = nacho_object[["nacho"]],
    id_colname = id_colname,
    count_column = "Count_Norm"
  )
  nacho_object[["normalised_counts"]] <- norm_counts

  return(nacho_object)
}

#' visualise
#'
#' @param nacho_object [nacho]
#' @param font_size [numeric]
#'
#' @return [NULL]
#' @export
#'
#' @examples
#' @importFrom shiny runApp
visualise <- function(nacho_object, font_size = 14) {
  if (missing(nacho_object)) {
    stop("No data provided!")
  }
  default_variables <- c(
    "access", "housekeeping_genes", "normalisation_method",
    "remove_outliers", "n_comp", "data_directory",
    "pc_sum", "nacho"
  )
  missing_variables <- any(is.na(match(x = default_variables, table = names(nacho_object))))
  if (missing_variables) {
    stop("No valid data provided. \n Use summarise() to generate data")
  }

  nacho_object[["font_size"]] <- font_size

  assign(x = "nacho_shiny", value = nacho_object, envir = .GlobalEnv) # Not good !!!

  app_directory <- system.file("shiny", package = "NACHO")
  shiny::runApp(appDir = app_directory)

  return(invisible())
}
