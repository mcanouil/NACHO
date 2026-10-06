app_html <- function(interactive, done = FALSE) {
  htmltools::renderTags(NACHO:::app_ui(
    done = done,
    interactive = interactive
  ))$html
}

accessible_name <- function(tag_html) {
  opening_tag <- sub("(?s)^(<button[^>]*>).*$", "\\1", tag_html, perl = TRUE)
  if (grepl('(aria-label|title)="[^"]+"', opening_tag)) {
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

test_that("every button has an accessible name", {
  for (interactive in c(FALSE, TRUE)) {
    html <- app_html(interactive, done = TRUE)
    buttons <- regmatches(
      html,
      gregexpr("(?s)<button[^>]*>.*?</button>", html, perl = TRUE)
    )[[1]]
    expect_gt(length(buttons), 5)
    named <- vapply(buttons, accessible_name, logical(1))
    expect_true(all(named), info = paste(buttons[!named], collapse = "\n"))
    expect_true(any(grepl(">Done<", buttons, fixed = TRUE)))
  }
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

test_that("popover bodies scroll instead of overflowing the viewport", {
  dependencies <- bslib::bs_theme_dependencies(NACHO:::nacho_theme())
  bootstrap <- Filter(function(x) x$name == "bootstrap", dependencies)[[1]]
  css <- paste(
    readLines(
      file.path(bootstrap$src$file, bootstrap$stylesheet),
      warn = FALSE
    ),
    collapse = ""
  )
  expect_match(css, "\\.popover-body\\s*\\{[^}]*max-height", perl = TRUE)
  expect_match(
    css,
    "\\.popover-body\\s*\\{[^}]*overflow-y:\\s*auto",
    perl = TRUE
  )
})

test_that("every interactive plot has a label and a text summary", {
  html <- app_html(TRUE)
  for (type in unlist(NACHO:::app_plot_types, use.names = FALSE)) {
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
  for (type in unlist(NACHO:::app_plot_types, use.names = FALSE)) {
    expect_match(html, paste0('id="', type, '-summary"'), fixed = TRUE)
  }
})

test_that("help text sits outside labels", {
  html <- app_html(FALSE)
  labels <- regmatches(
    html,
    gregexpr("<label[^>]*>.*?</label>", html, perl = TRUE)
  )[[1]]
  expect_false(any(grepl("help-block|form-text", labels)))
})

test_that("every static plot has alt text", {
  for (type in unlist(NACHO:::app_plot_types, use.names = FALSE)) {
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
