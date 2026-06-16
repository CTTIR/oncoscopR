#' Launch the zhncommandR Shiny dashboard
#'
#' Opens the auditor live-evaluation dashboard. With no data file uploaded
#' the app starts empty; the "Beispieldaten laden" button loads the
#' synthetic example cohort.
#'
#' @param ... Passed to [shiny::runApp()].
#'
#' @return Invisible `NULL`; called for the side effect of starting the
#'   Shiny app.
#'
#' @family app
#' @export
#' @examplesIf interactive()
#' zhn_run_app()
zhn_run_app <- function(...) {
  app_dir <- system.file("shiny", "zhncommandR", package = "zhncommandR")
  if (!nzchar(app_dir)) {
    cli::cli_abort(c(
      "App directory not found.",
      "i" = "Reinstall {.pkg zhncommandR}."
    ))
  }
  shiny::runApp(app_dir, ...)
}
