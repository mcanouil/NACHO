# Contributing to NACHO

Thanks for thinking about contributing to NACHO.
This page explains how to propose a change.

## Where to start

- Questions go in [Discussions, Q&A](https://github.com/mcanouil/NACHO/discussions/new?category=q-a).
- Ideas for new features go in [Discussions, Ideas](https://github.com/mcanouil/NACHO/discussions/new?category=ideas).
- Confirmed bugs go through the [bug report form](https://github.com/mcanouil/NACHO/issues/new?template=bug.yml), with a minimal [reprex](https://reprex.tidyverse.org/).

Before you start a substantial pull request, please open an issue or a discussion first, so we can agree it's worth doing.

## Fixing typos

You can fix small typos in the documentation straight from the GitHub web interface, as long as you edit the source file.

- Edit the roxygen comment in the `.R` file under `R/`.
- Don't edit the `.Rd` files under `man/`, because they are generated.
- Edit `README.Rmd`, not `README.md`, and render it with `devtools::build_readme()`.

## Development setup

NACHO uses [rig](https://rig.r-lib.org/) to manage the package library of the project.
Install rig, then run this command in the root of the repository:

```sh
rig proj sync
```

The command installs the dependencies from `rproj.toml` and NACHO itself into `.rvenv/`.
The `.Renviron` file of the repository tells R to use this library.
If you do not use rig, R shows `! Project is not synced. Run: rig proj sync` when it starts in the repository, and uses your usual library.

## Pull request process

- Create a Git branch for each pull request.
  Name it with a conventional commit type, for example `fix/plexset-detection`.
- Format R code with [Air](https://posit-dev.github.io/air/) (`air format .`), and please don't reformat code your change doesn't touch.
- Check the code with [lintr](https://lintr.r-lib.org/) (`lintr::lint_package()`) and fix what it reports in the lines you change.
- Document functions with [roxygen2](https://roxygen2.r-lib.org/) using Markdown, then run `devtools::document()`.
- Add or update tests with [testthat](https://testthat.r-lib.org/) (edition 3).
  Pull requests with tests are much easier to review.
- Run `devtools::check()` before you open the pull request.
- GitHub Actions runs `R CMD check` on macOS, Windows and Ubuntu for every pull request that isn't a draft.
- For user-facing changes, add an entry to `NEWS.md` under `# NACHO (development version)`.
  Entries sit under a section such as `## Fixes`, grouped by the file they change, with one `prefix: sentence.` item each:

  ```markdown
  - In `R/geometric_housekeeping.R`,
    - fix: replace background-corrected housekeeping counts below 1 with 1. ([#53](https://github.com/mcanouil/NACHO/issues/53))
  ```

## Code of Conduct

NACHO is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing, you agree to follow it.
