#' oncoscopR: Audit and Analysis Dashboard for Haematological Oncology Cohorts
#'
#' Reads tumour-documentation spreadsheets from haematological tumour centres
#' and computes quality and coverage indicators, OPS-8-544 complex chemotherapy
#' block counts, OPS-1-941 complex diagnostics counts, Kaplan-Meier analyses,
#' oncoprint and cytogenetics summaries, plus an interactive Shiny dashboard
#' for auditor live evaluation.
#'
#' The entry point for interactive use is [onc_run_app()]. Programmatic users
#' will mostly call the readers ([onc_read_cohort()], [onc_read_therapy()],
#' [onc_read_diagnostics()]) followed by the parsers
#' ([onc_prepare_therapy_blocks()], [onc_prepare_diagnostic_blocks()],
#' [onc_parse_oncoprint()], [onc_parse_cytogenetics()]).
#'
#' A fully synthetic example cohort is bundled and accessible via
#' [onc_example_path()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils read.csv write.csv
## usethis namespace: end
NULL

# These imports are consumed by inst/shiny/oncoscopR/app.R, which R CMD check
# does not scan. Declaring one symbol from each silences the
# "Namespaces in Imports field not imported from" note without changing
# behaviour.
#' @importFrom DT renderDT
#' @importFrom bslib bs_theme
#' @importFrom bsicons bs_icon
#' @importFrom ggplot2 ggplot
#' @importFrom scales percent
#' @importFrom survival survfit
NULL
