#' read_rcc
#'
#' @param file [character]
#'
#' @return [tibble]
read_rcc <- function(file) {
  tags <- c(
    "Header", "Sample_Attributes", "Lane_Attributes", "Code_Summary", "Messages"
  )
  raw <- readLines(file)
  raw <- sapply(raw, gsub, pattern = "[|]+[[:digit:]]+\\.*[[:digit:]]*", replacement = "")

  rcc_list <- lapply(X = tags, FUN = read_tags, raw_rcc = raw)
  names(rcc_list) <- tags

  rcc_list[["Code_Summary"]] <- purrr::map(.x = rcc_list[["Code_Summary"]], .f = function(.data) {
    colnames(.data) <- unlist(.data[1, ])
    rownames(.data) <- NULL
    .data <- .data[-1, ]
    if ("Count" %in% colnames(.data)) {
      .data[, "Count"] <- as.integer(.data[, "Count"])
    }
    if (all(paste0("Endogenous", 1:8, "s") %in% unique(.data[["CodeClass"]]))) {
      control_probes <- .data[.data[["CodeClass"]]%in%c("Negative", "Positive"), ]
      sample_list <- split(
        x = .data[!.data[["CodeClass"]]%in%c("Negative", "Positive"), ],
        f = .data[!.data[["CodeClass"]]%in%c("Negative", "Positive"), "CodeClass"]
      )
      .data <- lapply(X = sample_list, FUN = rbind, control_probes)
    }
    return(.data)
  })

  rcc_tbl <- tibble::as_tibble(rcc_list)

  if (length(rcc_tbl[["Code_Summary"]][[1]])==8) {
    column_to_unnest <- "Code_Summary"
    rcc_tbl <- tidyr::unnest(data = rcc_tbl, Code_Summary = get(column_to_unnest[1]), .drop = FALSE)
    rcc_tbl[["plexset_id"]] <- paste0("S", 1:8)
  }

  # remove NOTE: 'no visible binding for global variable' though it's a bit ugly
  column_to_unnest <- c("Header", "Sample_Attributes", "Lane_Attributes", "Messages")
  rcc_tbl <- tidyr::unnest(
    data = rcc_tbl,
    Header = get(column_to_unnest[1]),
    Sample_Attributes = get(column_to_unnest[2]),
    Lane_Attributes = get(column_to_unnest[3]),
    Messages = get(column_to_unnest[4]),
    .drop = FALSE
  )

  return(rcc_tbl)
}
