# S3 classes for the package's main tabular outputs.
#
# Each class inherits from data.frame so existing dplyr / base subscript code
# keeps working. Constructors are exported as new_*() (advanced users); the
# canonical way to obtain an instance is through the read_/parse_ pipelines.

# ---- cohort_df ------------------------------------------------------------

#' Cohort data frame
#'
#' A thin S3 wrapper around a data frame returned by [zhn_read_cohort()].
#' Inherits from `data.frame` so all base subscripting and dplyr verbs work;
#' the class is preserved across `[`-subsetting but stripped by most dplyr
#' verbs (use [tibble::as_tibble()] to opt out cleanly).
#'
#' @param x A data frame.
#' @param sheet_to_use Name of the workbook sheet the data came from
#'   (carried as an attribute).
#'
#' @return A `cohort_df` (S3 class), inheriting from `data.frame`.
#'
#' @family classes
#' @export
new_cohort_df <- function(x = data.frame(), sheet_to_use = NA_character_) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame.",
                   call = rlang::caller_env())
  }
  structure(
    x,
    class = c("cohort_df", "data.frame"),
    sheet_to_use = sheet_to_use
  )
}

#' @export
print.cohort_df <- function(x, ..., n = 5L) {
  cli::cli_h2("zhncommandR cohort")
  sheet <- attr(x, "sheet_to_use") %||% NA_character_
  cli::cli_bullets(c(
    "*" = "Rows: {.val {nrow(x)}}",
    "*" = "Columns: {.val {ncol(x)}}",
    "*" = "Source sheet: {.val {sheet}}"
  ))
  head_df <- as.data.frame(x)[seq_len(min(nrow(x), n)),
                              seq_len(min(ncol(x), 8L)),
                              drop = FALSE]
  print(head_df, row.names = FALSE)
  invisible(x)
}

#' @export
summary.cohort_df <- function(object, ...) {
  data.frame(
    rows           = nrow(object),
    columns        = ncol(object),
    diagnoses      = if ("diagnose" %in% names(object)) {
      .n_distinct_nonempty(object$diagnose)
    } else NA_integer_,
    years_covered  = if ("behandlungsjahr" %in% names(object)) {
      length(unique(stats::na.omit(object$behandlungsjahr)))
    } else NA_integer_,
    sheet_to_use   = attr(object, "sheet_to_use") %||% NA_character_,
    row.names      = NULL,
    stringsAsFactors = FALSE
  )
}

# ---- therapy_blocks -------------------------------------------------------

#' Therapy-blocks data frame
#'
#' S3 wrapper for the output of [zhn_prepare_therapy_blocks()].
#'
#' @inheritParams new_cohort_df
#' @param patient_cols_used Character; pipe-joined list of raw patient
#'   columns coalesced into `patient` (carried as an attribute).
#'
#' @family classes
#' @export
new_therapy_blocks <- function(x = data.frame(), patient_cols_used = "") {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame.",
                   call = rlang::caller_env())
  }
  structure(
    x,
    class = c("therapy_blocks", "data.frame"),
    patient_cols_used = patient_cols_used
  )
}

#' @export
print.therapy_blocks <- function(x, ...) {
  cli::cli_h2("OPS-8-544 therapy blocks")
  n_pat <- .n_distinct_nonempty(x$patient %||% character())
  n_prot <- .n_distinct_nonempty(x$therapieprotokoll %||% character())
  patient_cols <- attr(x, "patient_cols_used") %||% ""
  cli::cli_bullets(c(
    "*" = "Blocks: {.val {nrow(x)}}",
    "*" = "Patients: {.val {n_pat}}",
    "*" = "Protocols: {.val {n_prot}}",
    "*" = "Patient cols coalesced: {.val {patient_cols}}"
  ))
  invisible(x)
}

#' @export
summary.therapy_blocks <- function(object, ...) {
  data.frame(
    blocks    = nrow(object),
    patients  = .n_distinct_nonempty(object$patient %||% character()),
    protocols = .n_distinct_nonempty(object$therapieprotokoll %||% character()),
    diagnoses = .n_distinct_nonempty(object$diagnose %||% character()),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# ---- diagnostic_blocks ----------------------------------------------------

#' Diagnostic-blocks data frame
#'
#' S3 wrapper for the output of [zhn_prepare_diagnostic_blocks()].
#'
#' @inheritParams new_cohort_df
#' @param component_cols Character; comma-joined list of detected component
#'   columns (Morphologie / Immunphaenotypisierung / Zytogenetik /
#'   Molekulargenetik). Carried as an attribute.
#'
#' @family classes
#' @export
new_diagnostic_blocks <- function(x = data.frame(), component_cols = "") {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data frame.",
                   call = rlang::caller_env())
  }
  structure(
    x,
    class = c("diagnostic_blocks", "data.frame"),
    component_cols = component_cols
  )
}

#' @export
print.diagnostic_blocks <- function(x, ...) {
  cli::cli_h2("OPS-1-941 complex diagnostics")
  n_pat <- .n_distinct_nonempty(x$patient %||% character())
  components <- attr(x, "component_cols") %||% ""
  cli::cli_bullets(c(
    "*" = "Cases: {.val {nrow(x)}}",
    "*" = "Patients: {.val {n_pat}}",
    "*" = "Components: {.val {components}}"
  ))
  invisible(x)
}

#' @export
summary.diagnostic_blocks <- function(object, ...) {
  data.frame(
    cases     = nrow(object),
    patients  = .n_distinct_nonempty(object$patient %||% character()),
    diagnoses = .n_distinct_nonempty(object$diagnose %||% character()),
    components = attr(object, "component_cols") %||% "",
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# Util: rlang-style %||% used in print/summary above ---------------------------
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
