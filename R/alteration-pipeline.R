#' Shared body of the oncoprint + cytogenetics pipelines
#'
#' Splits a free-text column on commas/newlines, classifies each entry,
#' applies the negative filter, and deduplicates. Used by both
#' [zhn_parse_oncoprint()] and [zhn_parse_cytogenetics()].
#'
#' @inheritParams zhn_parse_oncoprint
#' @param source_col Character; name of the source free-text column.
#' @param diag_col Character; name of the diagnosis column.
#' @param patient_col Character or `NULL`; name of the patient column.
#' @param value_label Character; the name to give the raw-string column in
#'   the output. Typically `"alteration_raw"` or `"zytogenetik_raw"`.
#' @param distinct_extra `NULL` or character; extra column to include in
#'   the duplicate-detection key (`"oncoprint_mutation"` for the oncoprint
#'   pipeline).
#'
#' @keywords internal
.alteration_pipeline <- function(df, source_col, diag_col, patient_col,
                                 value_label, remove_negative,
                                 distinct_extra) {
  n <- nrow(df)
  tmp <- data.frame(
    .row_id = seq_len(n),
    patient_label = if (!is.null(patient_col)) {
      as.character(df[[patient_col]])
    } else {
      paste0("Fall_", seq_len(n))
    },
    diagnose_label = as.character(df[[diag_col]]),
    raw_full = as.character(df[[source_col]]),
    stringsAsFactors = FALSE
  )
  tmp <- tmp[!is.na(tmp$raw_full) & trimws(tmp$raw_full) != "", , drop = FALSE]
  if (nrow(tmp) == 0L) {
    return(.empty_alteration_table(
      include_oncoprint_flag = !is.null(distinct_extra),
      raw_name = value_label
    ))
  }

  # Split on commas and newlines only -- semicolons inside parens (e.g.
  # "t(11;14)") are part of cytogenetic notation, not a separator.
  tmp$raw_full <- stringr::str_replace_all(tmp$raw_full, "\\n|\\r|/", ",")
  tmp <- tidyr::separate_rows(tmp, "raw_full", sep = ",")
  tmp$raw_full <- trimws(tmp$raw_full)
  tmp$alteration <- zhn_normalize_alteration(tmp$raw_full)
  tmp$alteration_class <- zhn_alteration_type(tmp$raw_full)
  if (!is.null(distinct_extra)) {
    tmp$oncoprint_mutation <- zhn_is_mutation(tmp$alteration_class)
  }
  names(tmp)[names(tmp) == "raw_full"] <- value_label

  keep <- !is.na(tmp$alteration) & tmp$alteration != "" &
    !tolower(tmp$alteration) %in% c("na", "n.a.", "n/a", "nan", "null") &
    tmp$alteration_class != "Nicht verwertbar/NA"
  tmp <- tmp[keep, , drop = FALSE]

  if (isTRUE(remove_negative)) {
    tmp <- tmp[tmp$alteration_class != "negativ/kein Nachweis", , drop = FALSE]
  }

  dist_cols <- c("patient_label", "diagnose_label", "alteration",
                 "alteration_class")
  if (!is.null(distinct_extra)) dist_cols <- c(dist_cols, distinct_extra)
  if (value_label == "zytogenetik_raw") dist_cols <- c(dist_cols, value_label)
  tmp <- tmp[!duplicated(tmp[, dist_cols, drop = FALSE]), , drop = FALSE]
  rownames(tmp) <- NULL
  tmp$.row_id <- NULL
  tmp
}

# Empty contracts -----------------------------------------------------------

#' @keywords internal
.empty_therapy_blocks <- function() {
  out <- data.frame(
    therapieprotokoll = character(),
    diagnose = character(),
    patient = character(),
    datum = character(),
    zyklus = character(),
    jahr = integer(),
    monat_sort = character(),
    stringsAsFactors = FALSE
  )
  attr(out, "patient_cols_used") <- ""
  new_therapy_blocks(out)
}

#' @keywords internal
.empty_diagnostic_blocks <- function() {
  out <- data.frame(
    patient = character(),
    diagnose = character(),
    datum = character(),
    primaerfall = logical(),
    patientenfall = logical(),
    jahr = integer(),
    monat_sort = character(),
    stringsAsFactors = FALSE
  )
  attr(out, "component_cols") <- ""
  new_diagnostic_blocks(out)
}

#' @keywords internal
.empty_alteration_table <- function(include_oncoprint_flag,
                                    raw_name = "alteration_raw") {
  out <- data.frame(
    patient_label = character(),
    diagnose_label = character(),
    alteration = character(),
    alteration_class = character(),
    stringsAsFactors = FALSE
  )
  if (include_oncoprint_flag) out$oncoprint_mutation <- logical()
  out[[raw_name]] <- character()
  out
}

#' Attach year + monat_sort columns derived from a date column
#'
#' Detects whether the source is already Date/POSIXct (preferred) or
#' character; only round-trips to character when the source is character.
#'
#' @param blocks Data frame to augment.
#' @param date_col_name Name of the date column inside `blocks`.
#'
#' @keywords internal
.attach_year_month <- function(blocks, date_col_name) {
  d <- blocks[[date_col_name]]
  parsed_date <- if (inherits(d, "Date")) {
    d
  } else if (inherits(d, c("POSIXct", "POSIXt"))) {
    as.Date(d)
  } else {
    suppressWarnings(lubridate::as_date(d))
  }
  year_from_date <- suppressWarnings(lubridate::year(parsed_date))
  year_from_text <- suppressWarnings(
    as.integer(sub(".*(20[0-9]{2}).*", "\\1", as.character(d)))
  )
  bad_year <- is.na(year_from_text) | year_from_text < 2000 |
    year_from_text > 2100
  year_from_text[bad_year] <- NA_integer_
  blocks$jahr <- as.integer(ifelse(
    !is.na(year_from_date), year_from_date, year_from_text
  ))

  month_from_date <- ifelse(
    !is.na(parsed_date), format(parsed_date, "%Y-%m"), NA_character_
  )
  month_from_text <- ifelse(
    !is.na(blocks$jahr),
    paste0(blocks$jahr, "-",
           sprintf("%02d", suppressWarnings(
             as.integer(substr(as.character(d), 4, 5))
           ))),
    NA_character_
  )
  blocks$monat_sort <- ifelse(
    !is.na(month_from_date), month_from_date, month_from_text
  )
  blocks
}
