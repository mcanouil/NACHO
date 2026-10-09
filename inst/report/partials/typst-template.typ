// NACHO report frame: a navy cover, then A4 pages with a running header,
// a footer and a small pile of chips in the bottom-right corner.
// The chips and the header logo are artifacts, so screen readers skip them.
#import "partials/chips.typ": chip-pile

#let nacho-navy = rgb("#182430")
#let nacho-rust = rgb("#B64326")
#let nacho-amber = rgb("#FCB448")
#let nacho-muted = rgb("#5b6672")
#let nacho-line = rgb("#d9dee3")
#let nacho-tint = rgb("#f4f6f8")
#let nacho-cover-text = rgb("#c9d1d9")
#let nacho-cover-term = rgb("#9aa7b4")

// Quarto writes each callout as a call to `callout()` without its kind, so
// the icon colour tells the kinds apart: the brand primary or Quarto's blue
// for a note and Quarto's green for a tip give a navy edge; warning, caution
// and important give an amber edge.
// An amber callout also starts with "Warning:", so the kind does not rest on
// colour alone, as the hidden kind text does in HTML.
#let nacho-calm-callouts = (nacho-rust, rgb("#0758E5"), rgb("#00A047"))

#let callout(
  body: [],
  title: none,
  background_color: none,
  icon: none,
  icon_color: none,
  body_background_color: none,
) = {
  let warn = icon_color not in nacho-calm-callouts
  block(
    width: 100%,
    inset: 10pt,
    fill: if warn { rgb("#fff8eb") } else { nacho-tint },
    stroke: (
      left: 4pt + if warn { nacho-amber } else { nacho-navy },
      rest: 0.6pt + nacho-line,
    ),
  )[
    #if warn or title != none [*#if warn { "Warning: " }#title* \ ]
    #body
  ]
}

// The three counts of the decision summary, each as `(count, label, flag)`;
// the flagged box gets a rust top edge.
#let nacho-verdict(..boxes) = grid(
  columns: (1fr,) * boxes.pos().len(),
  gutter: 10pt,
  ..boxes.pos().map(((count, label, flag)) => block(
    width: 100%,
    inset: 10pt,
    stroke: (
      top: 3pt + if flag { nacho-rust } else { nacho-navy },
      rest: 0.6pt + nacho-line,
    ),
  )[
    #text(size: 20pt, weight: 700, count) \
    #text(size: 9pt, fill: nacho-muted, label)
  ]),
)

// The source of a parameter; a source the user chose is amber.
#let nacho-tag(user: false, body) = box(
  fill: if user { rgb("#fde9c6") } else { nacho-tint },
  inset: (x: 3pt, y: 1.5pt),
  outset: (y: 1pt),
  radius: 2pt,
  text(size: 0.85em, body),
)

// A value or a limit in a table.
#let nacho-num(body) = text(font: "JetBrains Mono", size: 0.9em, body)

#let nacho-report(
  title: none,
  author: none,
  prepared: none,
  details: (),
  generated: none,
  footer-text: none,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  doc,
) = {
  set document(title: title)
  set document(author: content-to-string(author)) if author != none
  set text(
    font: "Source Sans 3",
    size: 10.5pt,
    fill: nacho-navy,
    lang: "en",
    region: "us",
  )
  set par(justify: false, leading: 0.65em)
  show raw: set text(font: "JetBrains Mono")

  page(
    paper: "a4",
    margin: (x: 2.2cm, top: 4cm, bottom: 2.4cm),
    fill: nacho-navy,
    header: none,
    footer: none,
    background: none,
  )[
    #set text(fill: white)
    #block(stroke: (left: 5pt + nacho-amber), inset: (left: 1.2cm, y: 4pt))[
      #grid(
        columns: (auto, 1fr),
        gutter: 8pt,
        align: horizon,
        image("nacho_hex.png", height: 0.9cm, alt: "NACHO logo"),
        text(fill: nacho-cover-text, size: 11pt, tracking: 0.04em, smallcaps[NACHO quality-control report]),
      )
      #v(1.2cm)
      #show std.title: set text(size: 32pt, weight: 700)
      #std.title(title)
      #v(16pt)
      #text(fill: nacho-cover-text, size: 12pt, prepared)
      #v(1.4cm)
      #grid(
        columns: (auto,) * 3,
        column-gutter: 1.4cm,
        row-gutter: 14pt,
        ..details.map(((term, value)) => [
          #text(size: 8.5pt, fill: nacho-cover-term, term) \
          #text(size: 16pt, weight: 700, value)
        ]),
      )
    ]
    #place(
      bottom + right,
      dx: 2.2cm + 30pt,
      dy: 2.4cm + 30pt,
      pdf.artifact(chip-pile(scale: 1.3, alpha: 25%)),
    )
    #place(bottom + left, text(size: 8.5pt, fill: nacho-cover-term, generated))
  ]

  set page(
    paper: "a4",
    margin: (x: 2.2cm, top: 2.6cm, bottom: 2.4cm),
    fill: white,
    numbering: none,
    header: [
      #set text(size: 8.5pt, fill: nacho-muted)
      #grid(
        columns: (auto, auto, 1fr),
        column-gutter: 5pt,
        align: (left + horizon, left + horizon, right + horizon),
        pdf.artifact(image("nacho_hex.png", height: 0.55cm)),
        text(size: 10pt, fill: nacho-navy, weight: 700, tracking: 0.04em)[NACHO],
        title,
      )
      #v(-4pt)
      #line(length: 100%, stroke: 0.6pt + nacho-line)
    ],
    background: place(
      bottom + right,
      dx: 18pt,
      dy: 18pt,
      pdf.artifact(chip-pile(scale: 0.32, alpha: 55%)),
    ),
    footer: context [
      #set text(size: 8.5pt, fill: nacho-muted)
      #line(length: 100%, stroke: 0.6pt + nacho-line)
      #v(-4pt)
      #grid(
        columns: (1fr, auto),
        footer-text,
        [Page #counter(page).display() of #counter(page).final().first()],
      )
    ],
  )
  counter(page).update(1)

  set heading(numbering: sectionnumbering)
  show heading.where(level: 1): it => block(
    above: 1.6em,
    below: 0.8em,
    width: 100%,
    stroke: (bottom: 1.5pt + nacho-navy),
    inset: (bottom: 4pt),
  )[
    #set text(size: 17pt, weight: 700, fill: nacho-navy)
    #if it.numbering != none {
      text(fill: nacho-rust, counter(heading).display(it.numbering))
      h(4pt)
    }
    #it.body
  ]
  show heading.where(level: 2): set text(size: 13pt)

  show table: set align(left)
  show figure.where(kind: "quarto-float-tbl"): set block(breakable: true)
  set table(
    stroke: (x, y) => (
      bottom: if y == 0 { 1.2pt + nacho-navy } else { 0.5pt + nacho-line },
    ),
    inset: (x: 5pt, y: 5pt),
  )
  show figure.caption: it => text(size: 9pt, fill: nacho-muted)[
    *#it.supplement #context it.counter.display(it.numbering).* #it.body
  ]

  if toc {
    outline(title: if toc_title == none { auto } else { toc_title }, depth: toc_depth)
    pagebreak()
  }

  doc
}
