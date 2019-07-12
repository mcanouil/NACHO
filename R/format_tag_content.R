#' format_tag_content
#'
#' @inheritParams read_tags
#' @param content [data.frame] A `data.frame` created from RCC files.
#'
#' @keywords internal
#' @usage NULL
#'
#' @return [data.frame]
format_tag_content <- function(tag, content) {
  if (nrow(content)==1 & is.na(content[1, 1])) {
    output <- content
  } else {
    output <- as.data.frame(x = t(content[, -1, drop = FALSE]), stringsAsFactors = FALSE)
    rownames(output) <- NULL
    colnames(output) <- paste(tolower(gsub("_.*", "", tag)), content[, 1], sep = "_")
  }
  output
}