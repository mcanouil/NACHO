app_html <- function(interactive, done = FALSE) {
  htmltools::renderTags(NACHO:::app_ui(
    done = done,
    interactive = interactive
  ))$html
}

accessible_name <- function(tag_html) {
  opening_tag <- sub("(?s)^(<button[^>]*>).*$", "\\1", tag_html, perl = TRUE)
  if (grepl('\\saria-label="[^"]+"', opening_tag)) {
    return(TRUE)
  }
  if (
    grepl('class="[^"]*collapse-toggle', opening_tag) &&
      grepl('\\stitle="[^"]+"', opening_tag)
  ) {
    return(TRUE)
  }
  text <- gsub(
    "(?s)<i[^>]*>.*?</i>|<svg.*?</svg>|<[^>]+>|&nbsp;|\\s",
    "",
    tag_html,
    perl = TRUE
  )
  nzchar(text)
}

plot_types <- function() {
  types <- unlist(NACHO:::app_plot_types, use.names = FALSE)
  testthat::expect_gt(length(types), 0)
  types
}

threshold_html <- function() {
  htmltools::renderTags(NACHO:::threshold_inputs(
    NACHO::GSE74821,
    NACHO::GSE74821@thresholds,
    shiny::NS("thresholds")
  ))$html
}

button_tags <- function(html) {
  regmatches(
    html,
    gregexpr("(?s)<button[^>]*>.*?</button>", html, perl = TRUE)
  )[[1]]
}

expect_named_buttons <- function(interactive) {
  buttons <- button_tags(app_html(interactive, done = TRUE))
  testthat::expect_gt(length(buttons), 5)
  named <- vapply(buttons, accessible_name, logical(1))
  testthat::expect_true(
    all(named),
    info = paste(buttons[!named], collapse = "\n")
  )
  testthat::expect_true(any(grepl(">Done<", buttons, fixed = TRUE)))
}

test_that("every button in the static app has an accessible name", {
  expect_named_buttons(interactive = FALSE)
})

test_that("every button in the interactive app has an accessible name", {
  skip_if_not_installed("ggiraph")
  expect_named_buttons(interactive = TRUE)
})

test_that("every button in the threshold inputs has an accessible name", {
  buttons <- button_tags(threshold_html())
  expect_gt(length(buttons), 0)
  named <- vapply(buttons, accessible_name, logical(1))
  expect_true(all(named), info = paste(buttons[!named], collapse = "\n"))
  expect_true(all(grepl('aria-label="More about ', buttons, fixed = TRUE)))
})

test_that("every help popover trigger has an accessible name", {
  for (metric in names(NACHO:::threshold_pages)) {
    html <- htmltools::renderTags(NACHO:::threshold_help_block(metric))$html
    triggers <- regmatches(
      html,
      gregexpr("(?s)<button[^>]*>.*?</button>", html, perl = TRUE)
    )[[1]]
    expect_length(triggers, 1)
    expect_true(accessible_name(triggers), info = metric)
    expect_match(
      triggers,
      paste0('aria-label="More about ', NACHO:::qc_metric_labels[[metric]]),
      fixed = TRUE
    )
  }
})

test_that("help popovers scroll and carry the class that scopes the rule", {
  dependencies <- bslib::bs_theme_dependencies(NACHO:::nacho_theme())
  bootstrap <- Filter(function(x) x$name == "bootstrap", dependencies)[[1]]
  css <- paste(
    readLines(
      file.path(bootstrap$src$file, bootstrap$stylesheet),
      warn = FALSE
    ),
    collapse = ""
  )
  rule <- "\\.popover\\.nacho-help \\.popover-body\\s*\\{[^}]*"
  expect_match(css, paste0(rule, "max-height:\\s*70vh"), perl = TRUE)
  expect_match(css, paste0(rule, "overflow-y:\\s*auto"), perl = TRUE)
  expect_false(grepl(
    "(?<!nacho-help )\\.popover-body\\s*\\{[^}]*max-height",
    css,
    perl = TRUE
  ))
  expect_match(threshold_html(), "customClass", fixed = TRUE)
  expect_match(threshold_html(), "nacho-help", fixed = TRUE)
})

test_that("every interactive plot has a label and a text summary", {
  skip_if_not_installed("ggiraph")
  html <- app_html(TRUE)
  for (type in plot_types()) {
    expect_match(
      html,
      paste0('role="img" aria-label="', NACHO:::plot_alt_texts[[type]]),
      fixed = TRUE
    )
    expect_match(html, paste0('id="', type, '-summary"'), fixed = TRUE)
  }
})

test_that("every static plot card has a text summary", {
  html <- app_html(FALSE)
  for (type in plot_types()) {
    expect_match(html, paste0('id="', type, '-summary"'), fixed = TRUE)
  }
})

test_that("help text sits outside labels", {
  for (html in list(app_html(FALSE), threshold_html())) {
    labels <- regmatches(
      html,
      gregexpr("(?s)<label[^>]*>.*?</label>", html, perl = TRUE)
    )[[1]]
    expect_gt(length(labels), 0)
    expect_false(any(grepl("help-block|form-text", labels)))
  }
})

test_that("every static plot has alt text", {
  for (type in plot_types()) {
    shiny::testServer(
      NACHO:::mod_qc_plot_server,
      args = list(
        object = shiny::reactiveVal(GSE74821),
        qc = shiny::reactive(nacho_qc(GSE74821)),
        type = type,
        dark = shiny::reactive(FALSE)
      ),
      {
        session$setInputs(colour = "CartridgeID")
        session$returned()
        expect_identical(
          output$plot$alt,
          NACHO:::plot_alt_texts[[type]],
          info = type
        )
      }
    )
  }
})

test_that("the cite action is a named button the audit sees", {
  buttons <- button_tags(app_html(interactive = FALSE, done = TRUE))
  cite <- buttons[grepl('id="cite"', buttons, fixed = TRUE)]
  expect_length(cite, 1)
  expect_match(cite, 'type="button"', fixed = TRUE)
  expect_match(cite, "action-button", fixed = TRUE)
  expect_true(accessible_name(cite))
})

test_that("the external link and summary icons add no second name", {
  html <- app_html(interactive = FALSE, done = TRUE)
  expect_no_match(html, "arrow-up-right-from-square icon", fixed = TRUE)
  strip <- regmatches(
    html,
    regexpr(
      '(?s)<div class="nacho-summary".*?id="overview-preset"',
      html,
      perl = TRUE
    )
  )
  expect_length(strip, 1L)
  icons <- regmatches(strip, gregexpr("<i [^>]*>", strip))[[1]]
  expect_length(icons, 5L)
  expect_false(any(grepl("aria-label", icons, fixed = TRUE)))
})
