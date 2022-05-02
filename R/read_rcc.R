#' read_rcc
#'
#' @param file [[character]] The name of the RCC file which the data are to be read from.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[data.table]]
read_rcc <- function(file) {
  tags <- c(
    "Header", "Sample_Attributes", "Lane_Attributes", "Code_Summary", "Messages"
  )
  raw <- readLines(file)
  raw <- gsub("[|]+[[:digit:]]+\\.*[[:digit:]]*", "", raw)

  rcc_list <- lapply(X = tags, FUN = read_tags, raw_rcc = raw)
  names(rcc_list) <- tags

  rcc_list[["Code_Summary"]] <- lapply(X = rcc_list[["Code_Summary"]], FUN = function(.data) {
    colnames(.data) <- unlist(.data[1, ])
    rownames(.data) <- NULL
    .data <- .data[-1, ]
    if ("Count" %in% colnames(.data)) {
      .data[["Count"]] <- as.integer(.data[["Count"]])
    }
    if (all(paste0("Endogenous", seq_len(8), "s") %in% unique(.data[["CodeClass"]]))) {
      control_probes <- .data[.data[["CodeClass"]] %in% c("Negative", "Positive"), ]
      sample_list <- split(
        x = .data[!.data[["CodeClass"]] %in% c("Negative", "Positive"), ],
        f = gsub(
          "Endogenous|Housekeeping",
          "",
          .data[["CodeClass"]][!.data[["CodeClass"]] %in% c("Negative", "Positive")]
        )
      )
      .data <- lapply(X = sample_list, FUN = rbind, control_probes)
    }
    .data
  })

  rcc_dt <- data.table::as.data.table(rcc_list)

  if (length(rcc_dt[["Code_Summary"]][[1]]) == 8) {
    # column_to_unnest <- "Code_Summary"
    rcc_dt <- rcc_dt[
      j = list(
        Header,
        Sample_Attributes,
        Lane_Attributes,
        Code_Summary = unlist(Code_Summary, recursive = FALSE, use.names = FALSE),
        Messages,
        plexset_id = paste0("S", seq_len(8))
      )
    ]
  }

  # column_to_unnest <- c("Header", "Sample_Attributes", "Lane_Attributes", "Messages")
  rcc_dt[
    j = list(
      Header = data.table::rbindlist(Header),
      Sample_Attributes = data.table::rbindlist(Sample_Attributes),
      Lane_Attributes = data.table::rbindlist(Lane_Attributes),
      Code_Summary,
      Messages = gsub("messages_;", "", sapply(Messages, names)),
      plexset_id
    )
  ]
}
