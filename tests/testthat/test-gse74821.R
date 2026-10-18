test_that("GSE74821 carries no absolute file paths", {
  serialised <- serialize(GSE74821, NULL)
  patterns <- c("/Users", "/home", "/private", "Rtmp", "/tmp")
  hits <- vapply(
    patterns,
    function(pattern) {
      length(grepRaw(pattern, serialised, all = TRUE, fixed = TRUE))
    },
    integer(1)
  )
  expect_true(
    all(hits == 0),
    info = paste(
      "Found path patterns:",
      paste(names(hits)[hits > 0], collapse = ", ")
    )
  )
})

test_that("GSE74821 validates as a nacho object", {
  expect_no_error(S7::validate(GSE74821))
})
