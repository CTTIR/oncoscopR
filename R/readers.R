#' Resolve which Excel sheet to read for a given role
#'
#' Single source of truth for sheet selection across the three readers. The
#' resolution priority is:
#'
#' 1. Explicit `sheet` argument (if non-NULL and present in the file).
#' 2. A canonical name for the role.
#' 3. A role-specific regex fallback (legacy v5 behaviour).
#'
#' If none of those match, an informative error lists the available sheets.
#'
#' @param path Path to an `.xlsx` workbook.
#' @param role One of `"cohort"`, `"therapy"`, `"diagnostics"`.
#' @param sheet Optional explicit sheet name to use.
#'
#' @return Length-1 character: the matched sheet name.
#'
#' @keywords internal
.resolve_sheet <- function(path, role = c("cohort", "therapy", "diagnostics"),
                           sheet = NULL) {
  role <- match.arg(role)
  .check_file_exists(path)
  sheets <- readxl::excel_sheets(path)

  if (!is.null(sheet) && nzchar(sheet)) {
    if (!sheet %in% sheets) {
      cli::cli_abort(c(
        "Sheet {.val {sheet}} not found in {.path {basename(path)}}.",
        "i" = "Available sheets: {.val {sheets}}"
      ))
    }
    return(sheet)
  }

  canonical <- switch(role,
    cohort      = c("Basisdaten", "FaelleHAEZ", "Faelle", "Tabelle1"),
    therapy     = c("Komplexe Chemotherapie", "Therapie_OPS8544"),
    diagnostics = c("Komplexe Diagnostik")
  )
  hits <- canonical[canonical %in% sheets]
  if (length(hits) > 0L) return(hits[[1L]])

  regex <- switch(role,
    cohort      = "basisdaten|faelle|tabelle1",
    therapy     = "therapie|ops|8544|8[-_ ]?544|chemotherapie",
    diagnostics = "diagnostik|diagnostic|1[-_ ]?941|1941"
  )
  fallback <- sheets[grepl(regex, sheets, ignore.case = TRUE)]
  if (role == "diagnostics") {
    fallback <- setdiff(fallback, c("Basisdaten", "Komplexe Chemotherapie",
                                    "Faelle", "Tabelle1"))
  }
  if (role == "therapy") {
    fallback <- setdiff(fallback, c("Basisdaten", "Komplexe Diagnostik",
                                    "Faelle", "Tabelle1"))
  }
  if (length(fallback) > 0L) return(fallback[[1L]])

  cli::cli_abort(c(
    "No sheet matching role {.val {role}} found in {.path {basename(path)}}.",
    "i" = "Available sheets: {.val {sheets}}",
    "i" = "Pass an explicit {.code sheet =} argument to override."
  ))
}

#' Verify a file path is non-NULL and exists
#'
#' @keywords internal
.check_file_exists <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    cli::cli_abort("No file path supplied.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }
  invisible(TRUE)
}

#' Drop trailing empty `none*` columns and de-duplicate clean_names suffixes
#'
#' `janitor::clean_names()` turns duplicate Excel headers (e.g. two
#' `Erstdiagnose` columns) into `erstdiagnose` and `erstdiagnose_2`. The
#' Basisdaten sheet also carries trailing all-empty columns named `none`,
#' `none_2`, etc. This helper:
#'
#' * drops any `none*` column that is entirely empty;
#' * emits a `cli::cli_inform()` listing surviving `_2`/`_3` suffixed columns
#'   when `verbose = TRUE`, so the caller knows about the duplicates;
#' * is a no-op on data without `none*` columns or duplicate suffixes.
#'
#' @param df A data frame returned by `janitor::clean_names()`.
#' @param verbose Logical; emit a `cli_inform` describing what was dropped /
#'   what duplicated columns remain.
#'
#' @return The cleaned data frame.
#'
#' @keywords internal
.clean_duplicate_columns <- function(df, verbose = TRUE) {
  none_cols <- grep("^none(_\\d+)?$", names(df), value = TRUE)
  drop <- vapply(
    none_cols,
    function(col) {
      v <- df[[col]]
      if (is.character(v)) {
        all(is.na(v) | trimws(v) == "")
      } else {
        all(is.na(v))
      }
    },
    logical(1L)
  )
  to_drop <- none_cols[drop]
  if (length(to_drop) > 0L) {
    df <- df[, setdiff(names(df), to_drop), drop = FALSE]
  }
  dup_cols <- grep("_(\\d+)$", names(df), value = TRUE)
  if (verbose && length(dup_cols) > 0L) {
    cli::cli_inform(c(
      "i" = "De-duplicated columns kept with suffixes: {.val {dup_cols}}",
      "*" = "The first matching column wins where multiple were merged."
    ))
  }
  df
}

#' Read the cohort sheet (Basisdaten / Faelle)
#'
#' Reads the primary cohort sheet from a tumour-documentation workbook,
#' cleans column names via `janitor::clean_names()`, drops trailing empty
#' `none*` columns, and derives a `behandlungsjahr` (treatment year)
#' column from the first available date column.
#'
#' Replaces the legacy `read_main_data()` from v5 which scanned `~/Desktop`
#' for `Daten.xlsx`. The package never reads `~/Desktop`.
#'
#' @param path Path to the `.xlsx` file (typically a `fileInput()` temp path
#'   or the bundled example, obtained via [onc_example_path()]).
#' @param sheet Optional sheet name to override the canonical/regex resolver.
#' @param verbose Logical; emit a `cli_inform` when duplicate columns are
#'   detected.
#'
#' @return A data frame with cleaned names, an added `behandlungsjahr`
#'   integer column, and a `"sheet_to_use"` attribute. Empty input
#'   workbooks return a 0-row data frame.
#'
#' @family readers
#' @export
#' @examples
#' if (interactive()) {
#'   df <- onc_read_cohort(onc_example_path())
#'   head(df)
#' }
onc_read_cohort <- function(path, sheet = NULL, verbose = TRUE) {
  sheet_to_use <- .resolve_sheet(path, "cohort", sheet)
  df <- readxl::read_excel(path, sheet = sheet_to_use)
  df <- as.data.frame(df)
  df <- janitor::clean_names(df)
  df <- .clean_duplicate_columns(df, verbose = verbose)
  df <- .add_year(df)
  attr(df, "sheet_to_use") <- sheet_to_use
  df
}

#' Read the OPS-8-544 (complex chemotherapy) sheet
#'
#' Reads the therapy-block sheet — typically the canonical name
#' `Komplexe Chemotherapie` — from the workbook. If the sheet is genuinely
#' missing, returns a 0-row data frame with a `"source_label"` attribute
#' describing the situation; the dashboard surfaces this in the therapy tab.
#'
#' @inheritParams onc_read_cohort
#'
#' @return A data frame with cleaned names and a `"source_label"` attribute.
#'   0-row if no therapy sheet exists.
#'
#' @family readers
#' @export
onc_read_therapy <- function(path, sheet = NULL, verbose = TRUE) {
  out_label <- function(label) {
    out <- data.frame()
    attr(out, "source_label") <- label
    out
  }
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(out_label("Keine OPS-8-544-Tabelle gefunden"))
  }
  sheet_to_use <- tryCatch(
    .resolve_sheet(path, "therapy", sheet),
    error = function(e) NULL
  )
  if (is.null(sheet_to_use)) {
    return(out_label("Keine OPS-8-544-Tabelle gefunden"))
  }
  df <- readxl::read_excel(path, sheet = sheet_to_use)
  df <- as.data.frame(df)
  df <- janitor::clean_names(df)
  df <- .clean_duplicate_columns(df, verbose = verbose)
  attr(df, "source_label") <- paste0(
    "Quelle: ", basename(path), " / Blatt: ", sheet_to_use
  )
  attr(df, "sheet_to_use") <- sheet_to_use
  df
}

#' Read the OPS-1-941 (complex diagnostics) sheet
#'
#' Reads the diagnostics sheet — canonical name `Komplexe Diagnostik`.
#' If missing, returns a 0-row data frame with a `"source_label"` attribute.
#'
#' @inheritParams onc_read_cohort
#'
#' @return A data frame with cleaned names and a `"source_label"` attribute.
#'   0-row if no diagnostics sheet exists.
#'
#' @family readers
#' @export
onc_read_diagnostics <- function(path, sheet = NULL, verbose = TRUE) {
  out_label <- function(label) {
    out <- data.frame()
    attr(out, "source_label") <- label
    out
  }
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(out_label("Keine OPS-1-941-/Komplexe-Diagnostik-Tabelle gefunden"))
  }
  sheet_to_use <- tryCatch(
    .resolve_sheet(path, "diagnostics", sheet),
    error = function(e) NULL
  )
  if (is.null(sheet_to_use)) {
    return(out_label("Keine OPS-1-941-/Komplexe-Diagnostik-Tabelle gefunden"))
  }
  df <- readxl::read_excel(path, sheet = sheet_to_use)
  df <- as.data.frame(df)
  df <- janitor::clean_names(df)
  df <- .clean_duplicate_columns(df, verbose = verbose)
  attr(df, "source_label") <- paste0(
    "Quelle: ", basename(path), " / Blatt: ", sheet_to_use
  )
  attr(df, "sheet_to_use") <- sheet_to_use
  df
}

#' Read previously exported tumour-board decisions
#'
#' Reads a CSV file written by the dashboard's tumour-board download. Returns
#' an empty, typed data frame when the file is missing or empty — the
#' contract is the same in both cases so downstream `bind_rows` is safe.
#'
#' @param path Path to a CSV (typically from a `fileInput()` upload).
#'
#' @return A data frame with columns `Patient`, `Board_Datum`,
#'   `Tumorboardbeschluss`, `Verantwortlich`, `Erfasst_am`. Always returns
#'   this column set, even with 0 rows.
#'
#' @family readers
#' @export
onc_read_tumorboard <- function(path = NULL) {
  empty <- data.frame(
    Patient = character(),
    Board_Datum = as.Date(character()),
    Tumorboardbeschluss = character(),
    Verantwortlich = character(),
    Erfasst_am = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(empty)
  }
  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) empty
  )
  if (nrow(df) == 0L) return(empty)
  if ("Board_Datum" %in% names(df)) {
    df$Board_Datum <- suppressWarnings(as.Date(df$Board_Datum))
  }
  df
}
