# Releasing NACHO

Releases go to CRAN through three workflows.
Each one is started or approved by the maintainer, so nothing reaches CRAN by accident.

## 1. Bump the version

Run the "Release" workflow from `main` and pick `patch`, `minor` or `major`.

It updates `DESCRIPTION`, `NEWS.md`, `CITATION.cff` and `cran-comments.md`, checks spelling and URLs, and opens a pull request called "chore: release NACHO X.Y.Z".
The pull request merges straight away, because the bump can't break anything the checks cover.
The checks still run on `main`, and the submission workflow checks the tarball again before anything goes to CRAN.

Tick "rhub" to run the R-hub checks as well.
They're optional, and `cran-comments.md` only lists them when they run.

Untick "auto-merge" when `cran-comments.md` needs a note, for example to explain breaking changes or a NOTE you expect from the check.
The pull request is then assigned to you: edit the file on it and merge it yourself.

## 2. Submit to CRAN

Merging the bump starts the "CRAN submission" workflow.
It builds the tarball with the PDF manual and runs `R CMD check --as-cran`.

The workflow then compares the check with the results line of `cran-comments.md`, for example `0 errors | 0 warnings | 1 note`.
If they differ, it stops before the upload and lists the sections that raised a NOTE, a WARNING or an ERROR.
Open a pull request that explains each NOTE in `cran-comments.md` and corrects the results line, merge it, then run "CRAN submission" by hand from `main` with "dry run" unticked.

It then waits for your approval on the `cran` environment.
Approve it, and the workflow uploads the tarball and records the submission in `CRAN-SUBMISSION` on `main`.
CRAN then emails the maintainer, and the submission only counts once you click the link in that email.

Before the upload, the workflow checks `main` again.
If `CRAN-SUBMISSION` there already records this version, it stops, so a re-run never submits twice.

To rehearse without uploading, run "CRAN submission" by hand from `main` with "dry run" ticked.
It builds and checks the tarball of whatever version `main` holds, even a development one.
It still asks for approval on the `cran` environment, then stops before uploading.

If the upload step fails because devtools changed, submit from a local checkout with `devtools::submit_cran()`.

### When CRAN asks for changes

A rejected version stays recorded in `CRAN-SUBMISSION`, so the workflow won't submit it again on its own.

1. Open a pull request that fixes what CRAN asked for, deletes `CRAN-SUBMISSION`, and adds a "Resubmission" section to `cran-comments.md` saying what changed.
2. Merge it once the checks pass.
3. Run "CRAN submission" by hand from `main` with "dry run" unticked, then approve it as before.
   If the pull request changed `DESCRIPTION`, the merge has already started that workflow, so approve that run instead.

## 3. Publish the release

Once CRAN's acceptance email arrives, run the "CRAN post-release" workflow from `main`.

It checks that CRAN serves the version, tags `vX.Y.Z` on the submitted commit, and publishes the "NACHO X.Y.Z" release with the notes from `NEWS.md`.
The release deploys the pkgdown site.
A draft release `vX.Y.Z` stops the run, so publish or delete it before you run the workflow again.
It then opens a pull request that starts the next development version and removes `CRAN-SUBMISSION`.

If CRAN doesn't serve the version yet, the workflow stops without changing anything, and you can run it again later.

If a run stops after it published the release, run it again: it keeps the existing release when its tag points to the submitted commit, and goes on to the development version.

## Scripts and tests

The workflows call the scripts in `.github/scripts/`.
Run their tests from the repository root:

- `.github/scripts/tests/test-release-scripts.sh`
- `Rscript --vanilla .github/scripts/tests/test-cran-upload.R`
