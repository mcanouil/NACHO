source(file.path(".github", "scripts", "check-urls.R"))

problems <- data.frame(
  URL = c(
    "https://doi.org/10.1093/bioinformatics/btz647",
    "https://example.com/gone"
  ),
  Status = c("403", "404")
)

kept <- filter_url_problems(problems, c("^https://doi\\.org/", "", "  "))
stopifnot(
  nrow(kept) == 1L,
  identical(kept[["URL"]], "https://example.com/gone")
)

stopifnot(identical(filter_url_problems(problems, character()), problems))
stopifnot(nrow(filter_url_problems(problems[0L, ], "^https://")) == 0L)

cat("All check-urls checks passed\n")
