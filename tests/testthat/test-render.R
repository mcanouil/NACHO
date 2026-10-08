test_that("render() checks its object and options", {
  expect_error(render(), class = "nacho_error_bad_object")
  expect_error(render(iris), class = "nacho_error_bad_object")
  expect_error(
    render(GSE74821, format = "docx"),
    class = "nacho_error_bad_argument"
  )
  expect_error(
    render(GSE74821, colour = "nope"),
    class = "nacho_error_bad_argument"
  )
})

test_that("render() needs the quarto package", {
  local_mocked_bindings(has_package = function(package) package != "quarto")
  expect_error(render(GSE74821), class = "nacho_error_missing_package")
})

test_that("render() needs Quarto 1.9.18 or newer", {
  local_mocked_bindings(
    has_package = function(package) TRUE,
    quarto_cli_version = function() NULL
  )
  expect_error(render(GSE74821), class = "nacho_error_missing_quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.8.27")
  })
  expect_error(render(GSE74821), "1.8.27", class = "nacho_error_missing_quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.9.17")
  })
  expect_error(
    render(GSE74821),
    "it needs 1.9.18 or newer",
    class = "nacho_error_missing_quarto"
  )
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.9.18")
  })
  expect_true(quarto_available())
})

test_that("render() passes the library paths to Quarto and cleans up", {
  skip_if_not_installed("quarto")
  seen <- NULL
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, output_format, execute_params, ...) {
      seen <<- list(
        r_libs = Sys.getenv("R_LIBS"),
        quarto_r = Sys.getenv("QUARTO_R"),
        files = list.files(dirname(input), recursive = TRUE),
        params = execute_params
      )
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  withr::local_envvar(R_LIBS = "sentinel", QUARTO_R = "sentinel-r")
  output_dir <- withr::local_tempdir()
  path <- render(GSE74821, output_dir = output_dir)
  expect_identical(
    path,
    normalizePath(file.path(output_dir, "nacho-report.html"))
  )
  expect_true(file.exists(path))
  expect_identical(
    strsplit(seen$r_libs, .Platform$path.sep, fixed = TRUE)[[1]],
    .libPaths()
  )
  expect_identical(Sys.getenv("R_LIBS"), "sentinel")
  expect_identical(seen$quarto_r, R.home("bin"))
  expect_identical(Sys.getenv("QUARTO_R"), "sentinel-r")
  expect_true(all(
    c(
      "nacho-report.qmd",
      "nacho-report.scss",
      "accessible-tables.lua",
      "partials/title-block.html",
      "_brand.yml",
      "nacho_hex.png",
      "fonts/SourceSans3-Regular.ttf"
    ) %in%
      seen$files
  ))
  expect_identical(basename(seen$params$nacho_rds), "nacho.rds")
  expect_length(list.files(tempdir(), pattern = "^nacho-report-"), 0)
})

test_that("render() lets Quarto talk only when nacho.quiet is off", {
  skip_if_not_installed("quarto")
  seen <- NULL
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, quiet, ...) {
      seen <<- quiet
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  withr::local_options(nacho.quiet = FALSE)
  render(GSE74821, output_dir = withr::local_tempdir())
  expect_false(seen)
  withr::local_options(nacho.quiet = TRUE)
  render(GSE74821, output_dir = withr::local_tempdir())
  expect_true(seen)
})

test_that("render() tells when it cannot write to output_dir", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, ...) {
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  not_a_folder <- withr::local_tempfile()
  writeLines("a file", not_a_folder)
  expect_error(
    render(GSE74821, output_dir = not_a_folder),
    class = "nacho_error_render_failed"
  )
})

test_that("render() tells when Quarto writes nothing", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(...) invisible(),
    .package = "quarto"
  )
  expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
})

skip_unless_real_render <- function() {
  ready <- NACHO:::quarto_available() &&
    any(file.exists(file.path(.libPaths(), "NACHO", "DESCRIPTION")))
  if (ready) {
    return(invisible())
  }
  reason <- "Quarto or an installed NACHO is missing for the real render"
  if (identical(Sys.getenv("NACHO_REQUIRE_QUARTO"), "true")) {
    stop(reason, call. = FALSE)
  }
  testthat::skip(reason)
}

test_that("render() writes an HTML report", {
  skip_on_cran()
  skip_unless_real_render()
  output_dir <- withr::local_tempdir()
  path <- render(flagged_gse(), output_dir = output_dir)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "Flagged samples: [1-9]")
  expect_match(html, "callout-warning")
})

test_that("render() writes a Typst PDF report", {
  skip_on_cran()
  skip_unless_real_render()
  output_dir <- withr::local_tempdir()
  path <- render(GSE74821, format = "typst", output_dir = output_dir)
  expect_identical(basename(path), "nacho-report.pdf")
  expect_gt(file.size(path), 10000)
})

test_that("render() creates output_dir before it renders", {
  skip_if_not_installed("quarto")
  rendered <- FALSE
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, ...) {
      rendered <<- TRUE
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  output_dir <- file.path(withr::local_tempdir(), "a", "b")
  path <- render(GSE74821, output_dir = output_dir)
  expect_true(file.exists(path))
  not_a_folder <- withr::local_tempfile()
  writeLines("a file", not_a_folder)
  rendered <- FALSE
  expect_error(
    render(GSE74821, output_dir = not_a_folder),
    class = "nacho_error_render_failed"
  )
  expect_false(rendered)
})

test_that("render() shows the quiet hint only when output is quiet", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(...) invisible(),
    .package = "quarto"
  )
  withr::local_options(nacho.quiet = FALSE, rlib_message_verbosity = "default")
  loud <- expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
  expect_no_match(conditionMessage(loud), "nacho.quiet")
  withr::local_options(nacho.quiet = TRUE)
  quiet <- expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
  expect_match(conditionMessage(quiet), "rlib_message_verbosity")
})

test_that("render() does not create output_dir when the options are wrong", {
  output_dir <- file.path(withr::local_tempdir(), "report")
  expect_error(
    render(GSE74821, colour = "nope", output_dir = output_dir),
    class = "nacho_error_bad_argument"
  )
  expect_false(dir.exists(output_dir))
})

test_that("render() turns a Quarto failure into render_failed and cleans up", {
  skip_if_not_installed("quarto")
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(...) stop("boom"),
    .package = "quarto"
  )
  withr::local_options(nacho.quiet = TRUE)
  error <- expect_error(
    render(GSE74821, output_dir = withr::local_tempdir()),
    class = "nacho_error_render_failed"
  )
  expect_match(conditionMessage(error), "rlib_message_verbosity")
  expect_s3_class(error$parent, "error")
  expect_length(list.files(tempdir(), pattern = "^nacho-report-"), 0)
})

test_that("render() checks title and author", {
  expect_snapshot(render(GSE74821, title = 1), error = TRUE)
  expect_snapshot(render(GSE74821, author = c("a", "b")), error = TRUE)
  expect_snapshot(render(GSE74821, title = NA_character_), error = TRUE)
  expect_error(
    render(GSE74821, author = NA_character_),
    class = "nacho_error_bad_argument"
  )
})

test_that("render() passes the cover metadata to Quarto", {
  skip_if_not_installed("quarto")
  seen <- NULL
  local_mocked_bindings(quarto_cli_version = function() {
    numeric_version("1.10.18")
  })
  local_mocked_bindings(
    quarto_render = function(input, metadata, ...) {
      seen <<- metadata
      writeLines(
        "<html></html>",
        file.path(dirname(input), "nacho-report.html")
      )
    },
    .package = "quarto"
  )
  render(
    GSE74821,
    title = "Run A",
    author = "Jane Doe",
    output_dir = withr::local_tempdir()
  )
  expect_identical(seen$title, "Run A")
  expect_identical(seen$author, "Jane Doe")
  expect_match(seen$nacho$prepared, "^Prepared by Jane Doe")
})

cover_title <- 'Run "A": *x* <b>y</b> #1 $5'
cover_author <- "Micka\u00ebl Canouil, Lab & Co"

test_that("a real HTML render shows the title and author as typed", {
  skip_on_cran()
  skip_unless_real_render()
  path <- render(
    GSE74821,
    title = cover_title,
    author = cover_author,
    output_dir = withr::local_tempdir()
  )
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  escaped_title <- r"(Run &quot;A&quot;: *x* &lt;b&gt;y&lt;/b&gt; #1 $5)"
  expect_match(html, paste0("<title>", escaped_title, "</title>"), fixed = TRUE)
  expect_match(html, paste0(">", escaped_title, "</h1>"), fixed = TRUE)
  expect_match(html, "Micka\u00ebl Canouil, Lab &amp; Co", fixed = TRUE)
})

test_that("the HTML report has the cover, landmarks and accessible tables", {
  skip_on_cran()
  skip_unless_real_render()
  path <- render(
    GSE74821,
    group = "tissue type:ch1",
    title = "GSE74821",
    author = "Jane Doe",
    output_dir = withr::local_tempdir()
  )
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, '<html[^>]*lang="en-US"')
  expect_match(html, '<header[^>]*class="[^"]*nacho-cover')
  expect_match(html, "Prepared by Jane Doe", fixed = TRUE)
  expect_match(html, '<dl class="nacho-details">', fixed = TRUE)
  expect_match(html, "<dt>Samples</dt><dd>48</dd>", fixed = TRUE)
  expect_match(html, "<dt>Cartridges</dt><dd>4</dd>", fixed = TRUE)
  expect_match(html, "<dt>Normalization</dt><dd>GLM</dd>", fixed = TRUE)
  expect_match(
    html,
    '<p class="nacho-generated">Generated by NACHO [0-9.]+ on R [0-9.]+</p>'
  )
  expect_match(html, 'class="nacho-chips"[^>]*aria-hidden="true"')
  expect_match(html, '<main[^>]*id="quarto-document-content"')
  expect_match(html, 'href="#quarto-document-content"', fixed = TRUE)
  expect_match(html, '<nav[^>]*id="TOC"[^>]*aria-labelledby="toc-title"')
  expect_match(html, '<h2 id="toc-title">Contents</h2>', fixed = TRUE)
  ths <- regmatches(html, gregexpr("<th[ >][^>]*>", html))[[1]]
  expect_gt(length(ths), 0)
  expect_true(all(grepl('scope="col"', ths, fixed = TRUE)))
  expect_match(html, 'class="nacho-table-scroll"[^>]*aria-label="Table 1"')
  imgs <- regmatches(html, gregexpr("<img [^>]*>", html))[[1]]
  expect_true(all(grepl('alt="[^"]+"|aria-hidden="true"|alt=""', imgs)))
  expect_match(html, '<img [^>]*alt="NACHO logo"')
})

test_that("a real Typst render shows the title and author as typed", {
  skip_on_cran()
  skip_unless_real_render()
  skip_if(!nzchar(Sys.which("pdftotext")), "pdftotext is missing")
  path <- render(
    GSE74821,
    format = "typst",
    title = cover_title,
    author = cover_author,
    output_dir = withr::local_tempdir()
  )
  text <- paste(
    system2("pdftotext", c(path, "-"), stdout = TRUE),
    collapse = " "
  )
  text <- gsub("\\s+", " ", text)
  expect_match(text, cover_title, fixed = TRUE)
  expect_match(text, cover_author, fixed = TRUE)
})

pdf_text_pages <- function(path) {
  text <- paste(
    system2("pdftotext", c("-layout", path, "-"), stdout = TRUE),
    collapse = "\n"
  )
  strsplit(text, "\f", fixed = TRUE)[[1]]
}

pdf_inflated <- function(path) {
  bytes <- readBin(path, "raw", file.size(path))
  starts <- grepRaw("stream\r?\n", bytes, all = TRUE, value = FALSE)
  ends <- grepRaw("endstream", bytes, fixed = TRUE, all = TRUE)
  streams <- lapply(starts, function(start) {
    first <- start + if (bytes[start + 6L] == as.raw(13L)) 8L else 7L
    last <- ends[ends > first][1] - 1L
    if (is.na(last)) {
      return(raw())
    }
    tryCatch(memDecompress(bytes[first:last], "gzip"), error = function(e) {
      raw()
    })
  })
  list(file = bytes, streams = streams)
}

pdf_streams <- function(path) {
  inflated <- pdf_inflated(path)
  c(inflated$file, do.call(c, inflated$streams))
}

pdf_page_streams <- function(path) {
  streams <- Filter(
    function(stream) {
      length(stream) > 0 &&
        !any(stream == as.raw(0L)) &&
        length(grepRaw("BDC", stream, fixed = TRUE)) > 0
    },
    pdf_inflated(path)$streams
  )
  vapply(streams, rawToChar, character(1))
}

count_matches <- function(pattern, text) {
  vapply(
    gregexpr(pattern, text, perl = TRUE),
    function(match) sum(match > 0),
    integer(1)
  )
}

pdf_keys <- function(path) {
  lines <- system2("pdfinfo", path, stdout = TRUE)
  stats::setNames(
    trimws(sub("^[^:]+:", "", lines)),
    sub(":.*$", "", lines)
  )
}

test_that("the Typst report is a tagged PDF with the cover and decorations as artifacts", {
  skip_on_cran()
  skip_unless_real_render()
  skip_if(!nzchar(Sys.which("pdftotext")), "pdftotext is missing")
  skip_if(!nzchar(Sys.which("pdfinfo")), "pdfinfo is missing")
  path <- render(
    GSE74821,
    format = "typst",
    title = "GSE74821",
    author = "Jane Doe",
    output_dir = withr::local_tempdir()
  )
  keys <- pdf_keys(path)
  expect_identical(keys[["Title"]], "GSE74821")
  expect_identical(keys[["Author"]], "Jane Doe")
  expect_identical(keys[["Tagged"]], "yes")
  expect_identical(keys[["Page size"]], "595.276 x 841.89 pts (A4)")
  bytes <- pdf_streams(path)
  expect_gt(length(grepRaw("/StructTreeRoot", bytes, fixed = TRUE)), 0)
  expect_gt(length(grepRaw("pdfuaid:part>1<", bytes, fixed = TRUE)), 0)
  expect_gt(length(grepRaw("/Alt(NACHO logo)", bytes, fixed = TRUE)), 0)
  expect_gt(length(grepRaw("/Alt(", bytes, fixed = TRUE, all = TRUE)), 5)
  n_pages <- as.integer(keys[["Pages"]])
  streams <- pdf_page_streams(path)
  expect_length(streams, n_pages)
  header <- "(?s)/Subtype/Header/Type/Pagination>>BDC(?:(?!EMC).)*? Do"
  chips <- "(?s)/Artifact<<[^>]*/Type/Background>>BDC(?:(?!EMC).)*?/g[0-9]+ gs"
  cover_chips <- "/Artifact BMC\\s*q[^\\n]*/g[0-9]+ gs"
  is_cover <- count_matches("/Subtype/Header", streams) == 0
  expect_identical(sum(is_cover), 1L)
  expect_true(all(count_matches(header, streams[!is_cover]) == 1L))
  expect_true(all(count_matches(chips, streams[!is_cover]) == 1L))
  expect_identical(count_matches(cover_chips, streams[is_cover]), 1L)
  pages <- pdf_text_pages(path)
  cover <- gsub("\\s+", " ", pages[[1]])
  expect_match(
    cover,
    "NACHO QUALITY-CONTROL REPORT|NACHO quality-control report"
  )
  expect_match(cover, "Prepared by Jane Doe", fixed = TRUE)
  expect_match(cover, "Samples Cartridges Flagged 48 4 [0-9]+")
  expect_match(cover, "Generated by NACHO [0-9.]+ on R [0-9.]+")
  expect_no_match(cover, "Page [0-9]+ of")
  expect_match(pages[[2]], "NACHO", fixed = TRUE)
  expect_match(pages[[2]], "GSE74821", fixed = TRUE)
  last <- pages[[max(which(nzchar(trimws(pages))))]]
  expect_match(last, "Page ([0-9]+) of \\1")
  expect_match(
    last,
    "NACHO [0-9.]+ · R [0-9.]+ · [A-Z][a-z]+ [0-9]{1,2}, [0-9]{4}"
  )
})

test_that("a Typst render keeps a title and an author with special characters", {
  skip_on_cran()
  skip_unless_real_render()
  skip_if(!nzchar(Sys.which("pdftotext")), "pdftotext is missing")
  skip_if(!nzchar(Sys.which("pdfinfo")), "pdfinfo is missing")
  title <- 'Q "x" *b* #h $m$ \\ \u00e9'
  author <- 'Zo\u00eb "O\'B" *a* #1 $x$ \\ \u00d1'
  path <- render(
    GSE74821,
    format = "typst",
    title = title,
    author = author,
    output_dir = withr::local_tempdir()
  )
  keys <- pdf_keys(path)
  expect_identical(keys[["Title"]], title)
  expect_identical(keys[["Author"]], author)
  cover <- gsub("\\s+", " ", pdf_text_pages(path)[[1]])
  expect_match(cover, title, fixed = TRUE)
  expect_match(cover, paste("Prepared by", author), fixed = TRUE)
})

test_that("Typst callouts keep their title and get a navy or amber edge", {
  skip_on_cran()
  skip_if_not_installed("quarto")
  skip_if(is.null(quarto_cli_version()), "Quarto is missing")
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "partials"))
  file.copy(
    report_template_path("partials/chips.typ"),
    file.path(dir, "partials")
  )
  checks <- c(
    "#let edge(colour) = callout(body: [B], icon_color: colour).stroke.left.paint",
    "#assert.eq(edge(rgb(\"#b64326\")), nacho-navy)",
    "#assert.eq(edge(rgb(\"#0758E5\")), nacho-navy)",
    "#assert.eq(edge(rgb(\"#00A047\")), nacho-navy)",
    "#assert.eq(edge(rgb(\"#fcb448\")), nacho-amber)",
    "#assert.eq(edge(rgb(\"#EB9113\")), nacho-amber)",
    "#assert.eq(edge(rgb(\"#CC1914\")), nacho-amber)",
    "#assert.eq(edge(rgb(\"#FC5300\")), nacho-amber)",
    "#let kept = callout(title: [Kept title], body: [B], icon_color: nacho-rust)",
    "#assert(repr(kept.body).contains(\"Kept title\"))"
  )
  writeLines(
    c(
      "#let content-to-string(it) = \"\"",
      readLines(report_template_path("partials/typst-template.typ")),
      checks
    ),
    file.path(dir, "callout.typ")
  )
  status <- system2(
    quarto::quarto_path(),
    c("typst", "compile", "--root", dir, file.path(dir, "callout.typ")),
    stdout = FALSE,
    stderr = FALSE
  )
  expect_identical(status, 0L)
  writeLines(
    c(readLines(file.path(dir, "callout.typ")), "#assert.eq(1, 2)"),
    file.path(dir, "callout.typ")
  )
  status <- system2(
    quarto::quarto_path(),
    c("typst", "compile", "--root", dir, file.path(dir, "callout.typ")),
    stdout = FALSE,
    stderr = FALSE
  )
  expect_false(identical(status, 0L))
})
