#' norm_glm
#'
#' @param data [list(data.frame)]
#' @param exclude_probes [vector(character)]
#'
#' @return [vector(numeric)]
norm_glm <- function(data) {
  glms <- sapply(
    X = data,
    FUN = function(.data) {
      control_labels <- c("Positive", "Negative")
      .data <- .data[.data[["CodeClass"]] %in% control_labels, ]
      y <- .data[["Count"]] + 1
      check_name <- grepl("^[^(]*\\((.*)\\)$", .data[["Name"]])
      if (all(check_name)) {
        x <- as.numeric(gsub("^[^(]*\\((.*)\\)$", "\\1", .data[["Name"]]))
      } else {
        x <- c(NEG = 0, POS = 32)[gsub("([NP][EO][GS]).*", "\\1", .data[["Name"]])]
      }
      stats::glm(y ~ x, family = stats::poisson(link = "identity"))$coeff[c(1, 2)]
    }
  )

  list(
    geometric_mean_neg = glms[1, ],
    positive_factor = mean(glms[2, ]) / glms[2, ]
  )
}

