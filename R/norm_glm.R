#' norm_glm
#'
#' @param data [[data.frame]] A `list` of `data.frame` with the count data.
#'
#' @keywords internal
#' @usage NULL
#' @noRd
#'
#' @return [[list]]
norm_glm <- function(data) {
  glms <- sapply(
    X = data,
    FUN = function(.data) {
      control_labels <- c("Positive", "Negative")
      .data <- .data[.data[["CodeClass"]] %in% control_labels, ]
      y <- .data[["Count"]] + 1
      check_name <- grepl("^[^(]*\\((.*)\\)$", .data[["Name"]])
      if (all(check_name)) {
        x <- as.numeric(sub("^[^(]*\\((.*)\\)$", "\\1", .data[["Name"]]))
      } else {
        x <- c(NEG = 0, POS = 32)[sub("(NEG).*|(POS).*", "\\1\\2", .data[["Name"]])]
      }
      stats::glm(
        formula = y ~ x,
        family = stats::poisson(link = "identity"),
        data = data.frame(x, y)
      )$coeff[c(1, 2)]
    }
  )
  cat("\n")

  list(
    geometric_mean_neg = glms[1, ],
    positive_factor = mean(glms[2, ]) / glms[2, ]
  )
}
