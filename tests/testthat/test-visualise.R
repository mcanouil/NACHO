test_that("visualise() refuses to start outside an interactive session", {
  skip_if(interactive())
  expect_error(visualise(GSE74821), "interactive R session")
})

test_that("visualise() rejects an object that is not a nacho object", {
  expect_error(visualise(iris), "must be of class")
})

test_that("visualise() needs an object", {
  expect_error(visualise(), "is missing")
})

mandatory_fields <- c(
  "access",
  "housekeeping_genes",
  "housekeeping_predict",
  "housekeeping_norm",
  "normalisation_method",
  "remove_outliers",
  "n_comp",
  "data_directory",
  "pc_sum",
  "nacho",
  "outliers_thresholds"
)

for (field in mandatory_fields) {
  test_that(paste("visualise() needs the", field, "field"), {
    incomplete <- GSE74821
    incomplete[[field]] <- NULL
    expect_error(visualise(incomplete), "Mandatory fields are missing")
  })
}
