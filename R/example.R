#' Path to the bundled synthetic example workbook
#'
#' Returns the on-disk path of the synthetic example `.xlsx` shipped with
#' the package. The example is **100% fake data** — every name, diagnosis
#' and finding is invented for demonstration and testing.
#'
#' Used as the data source when the dashboard's "Beispieldaten laden"
#' button is clicked, and in the documentation examples.
#'
#' @return Length-1 character. The path to `onc_example.xlsx`.
#'
#' @family data
#' @export
#' @examples
#' onc_example_path()
#' if (interactive()) onc_read_cohort(onc_example_path())
onc_example_path <- function() {
  p <- system.file("extdata", "onc_example.xlsx", package = "oncoscopR")
  if (!nzchar(p)) {
    cli::cli_abort(c(
      "Bundled example workbook not found.",
      "i" = "Reinstall {.pkg oncoscopR} or run {.code source('data-raw/make_example_data.R')}."
    ))
  }
  p
}
