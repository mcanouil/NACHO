#' format_tag_content
#'
#' @param tag
#' @param content
#'
#' @return
#' @export
#'
#' @examples
format_tag_content <- function(tag, content) {
  if (nrow(content)==1 & is.na(content[1, 1])) {
    temp <- content
  } else {
    temp <- as.data.frame(x = t(content), stringsAsFactors = FALSE)
    temp <- temp[-1, ]
    rownames(temp) <- NULL
    colnames(temp) <- paste(tolower(gsub("_.*", "", tag)), content[, 1], sep = "_")
  }
  return(temp)
}