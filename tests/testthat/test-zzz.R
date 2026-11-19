test_that("no copies of external generics stay in the namespace", {
  ns <- asNamespace("NACHO")
  generics <- c("print", "format", "summary", "dim", "as.data.frame", "[")
  for (generic in generics) {
    expect_false(exists(generic, envir = ns, inherits = FALSE), label = generic)
  }
  expect_identical(get("autoplot", envir = ns), ggplot2::autoplot)
})

test_that("NACHO unloads and loads again with its methods", {
  skip_on_cran()
  skip_if(exists(".__DEVTOOLS__", envir = asNamespace("NACHO")))
  if ("package:NACHO" %in% search()) {
    withr::defer(library(NACHO))
  }
  expect_no_warning(unloadNamespace("NACHO"))
  expect_false(isNamespaceLoaded("NACHO"))
  expect_no_error(loadNamespace("NACHO"))
  expect_s3_class(
    ggplot2::autoplot(NACHO::GSE74821, type = "BD"),
    "ggplot"
  )
})
