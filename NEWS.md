# oncoscopR 0.1.0 (development)

* Initial CRAN-targeted release.
* Package extracts the ZHN Auditor Shiny app (v5) backend logic into
  reusable, namespaced functions: readers (`onc_read_cohort()`,
  `onc_read_therapy()`, `onc_read_diagnostics()`, `onc_read_tumorboard()`),
  parsers (`onc_prepare_therapy_blocks()`, `onc_prepare_diagnostic_blocks()`,
  `onc_parse_oncoprint()`, `onc_parse_cytogenetics()`), and alteration
  classification helpers (`onc_normalize_alteration()`,
  `onc_alteration_type()`, `onc_is_mutation()`).
* Dashboard launcher `onc_run_app()` plus bundled synthetic example data
  reachable through `onc_example_path()`.
* Fixed `as_event01("10 months")` no longer classifies as an event
  (legacy `grepl("^1", ...)` regression).
* Sheet-detection consolidated into a single resolver with explicit overrides
  and clear error messages listing available sheets.
* Duplicate columns post-`janitor::clean_names()` are detected and reported;
  trailing all-empty `none*` columns are dropped automatically.
* Tumour-board decisions are kept in-session (`reactiveVal`) with explicit
  CSV upload/download; no implicit writes to the filesystem.
