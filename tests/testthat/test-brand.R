test_that("the brand file holds the NACHO palette", {
  skip_if_not_installed("brand.yml")
  brand <- brand.yml::read_brand_yml(NACHO:::brand_path("_brand.yml"))
  expect_identical(
    unlist(brand$color$palette)[names(NACHO:::nacho_palette)],
    NACHO:::nacho_palette
  )
  expect_identical(brand.yml::brand_color_pluck(brand, "primary"), "#B64326")
  expect_identical(
    brand.yml::brand_color_pluck(brand, "foreground"),
    "#182430"
  )
})

test_that("every font the brand names ships as a TrueType file", {
  skip_if_not_installed("brand.yml")
  brand <- brand.yml::read_brand_yml(NACHO:::brand_path("_brand.yml"))
  paths <- unlist(lapply(brand$typography$fonts, function(font) {
    vapply(font$files, function(file) file$path, character(1))
  }))
  expect_length(paths, 5L)
  for (path in paths) {
    file <- NACHO:::brand_path(path)
    magic <- readBin(file, "raw", n = 4L)
    expect_identical(magic, as.raw(c(0x00, 0x01, 0x00, 0x00)), info = path)
  }
})

test_that("brand_path() names a missing brand file", {
  expect_error(
    NACHO:::brand_path("nope.yml"),
    class = "nacho_error_missing_file"
  )
  expect_true(file.exists(NACHO:::brand_path("nacho_hex.png")))
})

test_that("only the pairs the spec allows carry text", {
  skip_if_not_installed("colorspace")
  palette <- NACHO:::nacho_palette
  ratio <- function(fg, bg) colorspace::contrast_ratio(fg, bg)
  expect_gte(ratio("#FFFFFF", palette[["rust"]]), 4.5)
  expect_gte(ratio(palette[["navy"]], "#FFFFFF"), 4.5)
  expect_gte(ratio(palette[["amber"]], palette[["night"]]), 4.5)
  expect_gte(ratio(palette[["yellow"]], palette[["night"]]), 4.5)
  expect_gte(ratio("#FFFFFF", palette[["night"]]), 4.5)
  expect_gte(ratio(palette[["night"]], palette[["amber"]]), 4.5)
  expect_lt(ratio(palette[["rust"]], palette[["night"]]), 4.5)
  expect_lt(ratio(palette[["navy"]], palette[["rose"]]), 4.5)
})
