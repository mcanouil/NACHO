# Check the package URLs before a release, ignoring known false alarms.
# Each line of .github/url-check-ignore.txt is a regular expression; DOI and
# publisher redirects often fail from CI runners while working in a browser.

filter_url_problems <- function(problems, patterns) {
  patterns <- patterns[nzchar(trimws(patterns))]
  if (nrow(problems) == 0L || length(patterns) == 0L) {
    return(problems)
  }
  ignored <- vapply(
    problems[["URL"]],
    function(url) any(vapply(patterns, grepl, logical(1L), x = url)),
    logical(1L)
  )
  problems[!ignored, , drop = FALSE]
}

main <- function() {
  problems <- as.data.frame(urlchecker::url_check())
  patterns <- readLines(file.path(".github", "url-check-ignore.txt"))
  remaining <- filter_url_problems(problems, patterns)
  if (nrow(remaining) > 0L) {
    print(remaining[, c("URL", "Status", "Message"), drop = FALSE])
    quit(status = 1L)
  }
  cat("All URLs are fine\n")
}

if (sys.nframe() == 0L) {
  main()
}
