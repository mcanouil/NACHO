test_that("read_rcc() reads gzipped files and keeps the NACHO 2 attribute names", {
  parsed <- NACHO:::read_rcc(first_fixture_file("GSE178516"))
  expect_true(all(
    c(
      "Header.header_FileVersion",
      "Sample_Attributes.sample_Date",
      "Lane_Attributes.lane_FovCount",
      "Lane_Attributes.lane_BindingDensity"
    ) %in%
      names(parsed$attributes)
  ))
  expect_named(
    parsed$code_summary,
    c("CodeClass", "Name", "Accession", "Count")
  )
  expect_type(parsed$code_summary$Count, "integer")
})

test_that("read_rcc() reads the RCC FileVersion 2.0 miRNA files", {
  parsed <- NACHO:::read_rcc(first_fixture_file("GSE270837"))
  expect_identical(
    unname(parsed$attributes[["Header.header_FileVersion"]]),
    "2.0"
  )
  expect_gt(nrow(parsed$code_summary), 100)
})

test_that("read_rcc() matches section tags exactly", {
  lines <- readLines(first_fixture_file("GSE178516"))
  endogenous <- grep("^Endogenous,", lines)[1]
  lines[endogenous] <- sub(
    "^Endogenous,[^,]*,",
    "Endogenous,Messages,",
    lines[endogenous]
  )
  path <- withr::local_tempfile(fileext = ".RCC")
  writeLines(lines, path)
  parsed <- NACHO:::read_rcc(path)
  expect_true("Messages" %in% parsed$code_summary$Name)
})

test_that("read_rcc() reads CRLF files with trailing spaces", {
  lines <- readLines(first_fixture_file("GSE178516"))
  lines[grepl("^<", lines)] <- paste0(lines[grepl("^<", lines)], "  ")
  path <- withr::local_tempfile(fileext = ".RCC")
  writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), path)
  parsed <- NACHO:::read_rcc(path)
  expect_identical(
    parsed$code_summary,
    NACHO:::read_rcc(first_fixture_file("GSE178516"))$code_summary
  )
})

test_that("read_rcc() names the file and the section when a tag is missing", {
  path <- withr::local_tempfile(fileext = ".RCC")
  writeLines(c("<Header>", "FileVersion,1.7", "</Header>"), path)
  expect_error(NACHO:::read_rcc(path), class = "nacho_error_rcc_parse")
  expect_snapshot(
    NACHO:::read_rcc(path),
    error = TRUE,
    transform = function(x) sub(basename(path), "<file>", x, fixed = TRUE)
  )
})

test_that("rcc_samples() splits PlexSet files into eight samples with shared controls", {
  file <- list.files(
    test_path("plexset_data"),
    pattern = "\\.RCC$",
    full.names = TRUE
  )[1]
  samples <- NACHO:::rcc_samples(NACHO:::read_rcc(file))
  expect_named(samples, paste0("S", 1:8))
  expect_false(any(grepl("[0-8]s$", samples$S1$CodeClass)))
  expect_identical(
    samples$S1[samples$S1$CodeClass == "Positive", ],
    samples$S8[samples$S8$CodeClass == "Positive", ],
    ignore_attr = TRUE
  )
})

test_that("duplicated probe names within one file are an rcc_parse error", {
  lines <- readLines(first_fixture_file("GSE178516"))
  endogenous <- grep("^Endogenous,", lines)[1:2]
  lines[endogenous[2]] <- lines[endogenous[1]]
  path <- withr::local_tempfile(fileext = ".RCC")
  writeLines(lines, path)
  expect_error(
    NACHO:::rcc_samples(NACHO:::read_rcc(path)),
    class = "nacho_error_rcc_parse"
  )
})
