#' Launch the zhncommandR Shiny dashboard
#'
#' Opens the auditor live-evaluation dashboard. With no data file uploaded
#' the app starts empty; the "Beispieldaten laden" button loads the
#' synthetic example cohort.
#'
#' The app is copied to a session temporary directory and run from there.
#' [shiny::runApp()] sets the working directory to the app folder, so running
#' it directly from the package library would keep that folder open and block
#' updating or reinstalling the package on Windows ("device or resource busy")
#' while the app is running. Running from a temp copy avoids that lock; the
#' package's own files (translations, example data) are still read from the
#' installed library via [system.file()].
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
  run_dir <- file.path(tempdir(), "zhncommandR-app")
  unlink(run_dir, recursive = TRUE, force = TRUE)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(
    list.files(app_dir, full.names = TRUE), run_dir,
    recursive = TRUE, overwrite = TRUE
  )
  # Fall back to the installed dir if the copy could not be made.
  shiny::runApp(if (all(ok)) run_dir else app_dir, ...)
}
