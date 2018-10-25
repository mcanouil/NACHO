#' norm_glm
#'
#' @param data [list(data.frame)]
#' @param exclude_probes [vector(character)]
#'
#' @return [vector(numeric)]
#'
#' @importFrom stats glm poisson
norm_glm <- function(data, exclude_probes = NULL) {
  glms <- sapply(
    X = data,
    exclude_probes = exclude_probes,
    FUN = function(.data, exclude_probes) {
      control_labels <- c("Positive", "Negative")

      control_data <- as.data.frame(.data[, c("Name", "CodeClass", "Count")])
      control_data <- control_data[control_data[["CodeClass"]] %in% control_labels, ]
      control_data <- control_data[order(control_data[, "Name"]), ]
      control_data <- control_data[!control_data[, "Name"] %in% exclude_probes, ]

      y <- as.numeric(control_data[, "Count"]) + 1
      if (all(grepl("^[^(]*\\((.*)\\)$", control_data[, "Name"]))) {
        x <- as.numeric(gsub("^[^(]*\\((.*)\\)$", "\\1", control_data[, "Name"]))
      } else {
        x <- sapply(X = control_data[, "Name"], FUN = function(iprobe) {
          if (grepl("NEG", iprobe)) {0} else if (grepl("POS", iprobe)) {32}
        })
      }
      model <- stats::glm(y ~ x, family = stats::poisson(link = identity))
      output <- c(
        "intercept" = unname(model$coeff[1]),
        "slope" = unname(model$coeff[2])
      )
      return(output)
    }
  )
  geometric_mean_neg <- glms["intercept", ]
  slopes <- glms["slope", ]
  positive_factor <- sapply(X = slopes, FUN = function(x) {mean(slopes) / x})
  return(positive_factor)
}
