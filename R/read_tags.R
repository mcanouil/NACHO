#' read_tags
#'
#' @param tag [character] A \code{character} string naming the TAG field in the RCC files.
#' @param raw_rcc [vector(character)]  A \code{vector(character)} containing all lines from a RCC file.
#'
#' @keywords internal
#'
#' @return [list(data.frame)]
read_tags <- function(tag, raw_rcc) {
  positions <- grep(pattern = tag, x = raw_rcc)
  if ((positions[2]-positions[1])!=1) {
    content_list <- raw_rcc[(positions[1] + 1):(positions[2] - 1)]
    tag_content <- do.call("rbind", strsplit(content_list, ","))
  } else {
    tag_content <- matrix(NA, nrow = 1, ncol = 1, dimnames = list(NULL, tag))
  }

  if (tag!="Code_Summary") {
    tag_content <- format_tag_content(tag = tag, content = tag_content)
  } else {
    tag_content <- as.data.frame(tag_content, stringsAsFactors = FALSE)
  }

  list(tag_content)
}
