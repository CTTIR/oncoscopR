# cran-comments.md

## Submission

This is the initial submission of `oncoscopR` 0.1.0.

## Test environments

* local macOS, R 4.4.x
* GitHub Actions: ubuntu-latest (release, devel, oldrel-1), macos-latest,
  windows-latest

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes for CRAN

* The package bundles a small synthetic example workbook
  (`inst/extdata/onc_example.xlsx`, well under the recommended size
  limit). All data is invented; no real patient records are included.
* `survminer` is in `Suggests` and gated behind a `requireNamespace()`
  check inside the Shiny app to avoid pulling its heavy dependency
  tree into `Imports`. A base-ggplot KM fallback handles the case
  where `survminer` is unavailable.
* The Shiny app never writes outside `tempdir()`. Tumour-board
  decisions are stored in-session and exported via a download handler.
