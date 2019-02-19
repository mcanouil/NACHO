#' format_tag_content
#'
#' @param tag [character]
#' @param content [data.frame]
#'
#' @return [data.frame]
format_tag_content <- function(tag, content) {
  if (nrow(content)==1 & is.na(content[1, 1])) {
    output <- content
  } else {
    output <- as.data.frame(x = t(content), stringsAsFactors = FALSE)
    output <- output[-1, ]
    rownames(output) <- NULL
    colnames(output) <- paste(tolower(gsub("_.*", "", tag)), content[, 1], sep = "_")
  }
  output
}