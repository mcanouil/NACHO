# Getting help with NACHO

Thanks for using NACHO. Here’s where to go, depending on what you need.

## Read the documentation

The documentation site has the reference for every function and the
vignettes: <https://m.canouil.dev/NACHO/>. Changes that aren’t released
yet are documented at <https://m.canouil.dev/NACHO/dev/>.

## Ask a question

Questions about using NACHO go in [Discussions,
Q&A](https://github.com/mcanouil/NACHO/discussions/new?category=q-a).
Please search the existing discussions first, as someone may have asked
already.

## Suggest an idea

Ideas for new features go in [Discussions,
Ideas](https://github.com/mcanouil/NACHO/discussions/new?category=ideas).

## Report a bug

Before you report a bug, please check it still happens with the latest
version. You can install the CRAN release with
`install.packages("NACHO")`, or the development version with
`pak::pak("mcanouil/NACHO")`.

Then open an issue with the [bug report
form](https://github.com/mcanouil/NACHO/issues/new?template=bug.yml). It
helps a lot if you include:

- The output of `packageVersion("NACHO")`.
- A minimal reproducible example, made with
  [reprex](https://reprex.tidyverse.org/) if you can.
- Whether the problem happens in the Shiny application or in the R
  functions.
- Public or anonymised data, such as the GEO series used in
  [`?load_rcc`](https://m.canouil.dev/NACHO/dev/reference/load_rcc.md).
- The output of
  [`sessioninfo::session_info()`](https://sessioninfo.r-lib.org/reference/session_info.html)
  or [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html).

## Report a conduct or security concern

Please don’t open a public issue for these. Email Mickaël Canouil at
<pro@mickael.canouil.dev> instead, as described in the [Code of
Conduct](https://m.canouil.dev/NACHO/dev/CODE_OF_CONDUCT.md).
