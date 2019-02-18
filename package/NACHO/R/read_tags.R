#' read_tags
#'
#' @param tag [character]
#' @param raw_rcc [vector(character)]
#'
#' @return [list(data.frame)]
read_tags <- function(tag, raw_rcc) {
  positions <- grep(pattern = tag, x = raw_rcc)
  if ((positions[2]-positions[1])!=1) {
    content_list <- raw_rcc[(positions[1] + 1):(positions[2] - 1)]
    td_content_list <- lapply(
      X = content_list,
      FUN = function(vec) {unique(unlist(strsplit(vec, ",")))}
    )
    tag_content <- do.call(rbind, td_content_list)
    tag_content <- as.data.frame(tag_content, stringsAsFactors = FALSE)
  } else {
    tag_content <- as.data.frame(
      x = matrix(NA, nrow = 1, ncol = 1, dimnames = list(NULL, tag)),
      stringsAsFactors = FALSE
    )
  }

  if (tag!="Code_Summary") {
    tag_content <- format_tag_content(tag = tag, content = tag_content)
  }
  return(list(tag_content))
}
