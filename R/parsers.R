#' Prepare OPS-8-544 therapy blocks for downstream analysis
#'
#' Reshapes the raw therapy sheet (as returned by [onc_read_therapy()]) into
#' the per-block tibble the dashboard counts. Counts every numeric
#' OPS-8-544 entry > 0 as a positive block; falls back to deduplicating
#' Patient/Datum/Zyklus/Protokoll combinations when no OPS column exists.
#'
#' @param df Data frame returned by [onc_read_therapy()].
#'
#' @return Data frame with at minimum:
#'   `therapieprotokoll`, `diagnose`, `patient`, `datum`, `zyklus`, `jahr`,
#'   `monat_sort`. Empty input returns a 0-row tibble with this column set.
#'   Attribute `patient_cols_used` records which raw columns were coalesced
#'   into `patient`.
#'
#' @family parsers
#' @export
onc_prepare_therapy_blocks <- function(df) {
  empty <- .empty_therapy_blocks()
  if (is.null(df) || nrow(df) == 0L) return(empty)

  ops_col     <- .find_col(df, c("ops8544", "ops_8544", "ops8_544", "ops_8_544",
                                 "ops_8_544_komplexe_chemotherapie"))
  prot_col    <- .find_col(df, c("therapieprotokoll", "therapie_protokoll",
                                 "protokoll", "therapie", "schema", "regime"))
  diag_col    <- .find_col(df, c("diagnose", "kodierung", "entitaet",
                                 "entitaet_onkozert"))
  patient_col <- .find_col(df, c("patient", "patient_1", "patient_2", "name",
                                 "patient_name", "patient_id", "patienten_id",
                                 "kis_patienten_id", "id"))
  date_col    <- .find_col(df, c("datum", "herstellungsdatum", "therapiedatum",
                                 "applikationsdatum", "beginn"))
  cycle_col   <- .find_col(df, c("zyklusnr", "zyklus", "zyklus_nr", "block",
                                 "block_nr"))

  if (!is.null(ops_col)) {
    if (is.numeric(df[[ops_col]])) {
      df$.ops_event <- ifelse(
        is.na(df[[ops_col]]), NA_real_,
        ifelse(df[[ops_col]] > 0, 1, 0)
      )
    } else {
      ops_chr <- trimws(as.character(df[[ops_col]]))
      ops_num <- suppressWarnings(as.numeric(gsub(",", ".", ops_chr)))
      df$.ops_event <- ifelse(
        !is.na(ops_num),
        ifelse(ops_num > 0, 1, 0),
        .as_event01(df[[ops_col]], mode = "auto")
      )
    }
    if (any(df$.ops_event == 1, na.rm = TRUE)) {
      blocks <- df[!is.na(df$.ops_event) & df$.ops_event == 1, , drop = FALSE]
    } else {
      keep <- unique(stats::na.omit(c(patient_col, date_col, cycle_col,
                                      prot_col, diag_col)))
      blocks <- unique(df[, keep, drop = FALSE])
    }
  } else {
    keep <- unique(stats::na.omit(c(patient_col, date_col, cycle_col,
                                    prot_col, diag_col)))
    if (length(keep) == 0L) return(empty)
    blocks <- unique(df[, keep, drop = FALSE])
  }

  get_chr <- function(data, col, default = NA_character_) {
    if (is.null(col) || !(col %in% names(data))) {
      rep(default, nrow(data))
    } else {
      as.character(data[[col]])
    }
  }

  blocks$therapieprotokoll <- get_chr(blocks, prot_col, "Nicht angegeben")
  blocks$diagnose <- get_chr(blocks, diag_col, "Nicht angegeben")

  patient_name_cols <- intersect(
    c("patient", "patient_1", "patient_2", "name", "patient_name"),
    names(blocks)
  )
  patient_id_cols <- intersect(
    c("patienten_id", "kis_patienten_id", "patient_id", "id"),
    names(blocks)
  )
  blocks$patient <- .first_nonempty_col(
    blocks, c(patient_name_cols, patient_id_cols)
  )
  attr(blocks, "patient_cols_used") <- paste(
    c(patient_name_cols, patient_id_cols), collapse = ", "
  )

  blocks$datum <- get_chr(blocks, date_col, NA_character_)
  blocks$zyklus <- get_chr(blocks, cycle_col, NA_character_)

  empty_prot <- is.na(blocks$therapieprotokoll) |
    trimws(blocks$therapieprotokoll) == ""
  blocks$therapieprotokoll[empty_prot] <- "Nicht angegeben"
  empty_diag <- is.na(blocks$diagnose) | trimws(blocks$diagnose) == ""
  blocks$diagnose[empty_diag] <- "Nicht angegeben"

  blocks <- .attach_year_month(blocks, "datum")
  blocks
}

#' Prepare OPS-1-941 complex-diagnostics blocks
#'
#' @param df Data frame returned by [onc_read_diagnostics()].
#'
#' @return Data frame with `patient`, `diagnose`, `datum`, `primaerfall`,
#'   `patientenfall`, `jahr`, `monat_sort`, plus the component columns
#'   (Morphologie, Immunphaenotypisierung, Zytogenetik, Molekulargenetik).
#'   Empty input returns a 0-row tibble.
#'
#' @family parsers
#' @export
onc_prepare_diagnostic_blocks <- function(df) {
  empty <- .empty_diagnostic_blocks()
  if (is.null(df) || nrow(df) == 0L) return(empty)

  patient_col <- .find_col(df, c("patient", "name", "patient_name",
                                 "patient_id", "patienten_id",
                                 "kis_patienten_id", "id"))
  diag_col    <- .find_col(df, c("diagnose", "kodierung", "entitaet",
                                 "entitaet_onkozert", "diagnose_kategorie"))
  ops_col     <- .find_col(df, c("komplexe_diagnostik", "ops_1_941",
                                 "ops1941", "ops_1941", "ops1_941"))
  date_col    <- .find_col(df, c("tumorkonferenz", "tumorkonferenz_datum",
                                 "erstvorstellung", "erstdiagnose", "datum"))
  primary_col <- .find_col(df, c("primarfall", "primaerfall"))
  case_col    <- .find_col(df, c("patientenfall"))

  if (!is.null(ops_col)) {
    if (is.numeric(df[[ops_col]])) {
      df$.diag_event <- ifelse(
        is.na(df[[ops_col]]), NA_real_,
        ifelse(df[[ops_col]] > 0, 1, 0)
      )
    } else {
      ops_chr <- trimws(as.character(df[[ops_col]]))
      ops_num <- suppressWarnings(as.numeric(gsub(",", ".", ops_chr)))
      df$.diag_event <- ifelse(
        !is.na(ops_num),
        ifelse(ops_num > 0, 1, 0),
        .as_event01(df[[ops_col]], mode = "auto")
      )
    }
    blocks <- df[!is.na(df$.diag_event) & df$.diag_event == 1, , drop = FALSE]
  } else {
    blocks <- df
  }

  if (nrow(blocks) == 0L) return(empty)

  get_chr <- function(data, col, default = NA_character_) {
    if (is.null(col) || !(col %in% names(data))) {
      rep(default, nrow(data))
    } else {
      as.character(data[[col]])
    }
  }

  blocks$patient <- get_chr(blocks, patient_col, NA_character_)
  blocks$diagnose <- get_chr(blocks, diag_col, "Nicht angegeben")
  empty_diag <- is.na(blocks$diagnose) | trimws(blocks$diagnose) == ""
  blocks$diagnose[empty_diag] <- "Nicht angegeben"
  blocks$datum <- get_chr(blocks, date_col, NA_character_)
  blocks$primaerfall <- if (!is.null(primary_col)) {
    .as_yesno(blocks[[primary_col]])
  } else {
    rep(NA, nrow(blocks))
  }
  blocks$patientenfall <- if (!is.null(case_col)) {
    .as_yesno(blocks[[case_col]])
  } else {
    rep(NA, nrow(blocks))
  }

  comp_candidates <- names(blocks)[
    grepl("morph|immun|zyto|cyto|molekular|molecular|genetik|diagnostik",
          names(blocks), ignore.case = TRUE)
  ]
  comp_candidates <- setdiff(comp_candidates, c(ops_col, ".diag_event"))
  attr(blocks, "component_cols") <- paste(comp_candidates, collapse = ", ")

  blocks <- .attach_year_month(blocks, "datum")
  blocks
}

#' Pivot diagnostic-block components into long form
#'
#' Detects component columns (Morphologie/Immunphaenotypisierung/Zytogenetik/
#' Molekulargenetik), pivots them long, normalises the display label, and
#' filters to positive entries only.
#'
#' @param blocks Tibble from [onc_prepare_diagnostic_blocks()].
#'
#' @return Long tibble with `patient`, `diagnose`, `jahr`, `monat_sort`,
#'   `diagnostik_bereich`, `wert`, `positiv`. Empty input returns a 0-row
#'   tibble.
#'
#' @keywords internal
.diagnostic_components_long <- function(blocks) {
  empty <- data.frame(
    patient = character(), diagnose = character(),
    jahr = integer(), monat_sort = character(),
    diagnostik_bereich = character(), wert = character(),
    positiv = logical(), stringsAsFactors = FALSE
  )
  if (is.null(blocks) || nrow(blocks) == 0L) return(empty)

  comp_cols <- names(blocks)[
    grepl("morph|immun|zyto|cyto|molekular|molecular|genetik",
          names(blocks), ignore.case = TRUE)
  ]
  if (length(comp_cols) == 0L) return(empty)

  long <- tidyr::pivot_longer(
    blocks[, intersect(c("patient", "diagnose", "jahr", "monat_sort",
                         comp_cols), names(blocks)), drop = FALSE],
    cols = dplyr::all_of(comp_cols),
    names_to = "diagnostik_bereich",
    values_to = "wert"
  )
  long$positiv <- vapply(long$wert, function(v) {
    if (is.numeric(v)) return(!is.na(v) & v > 0)
    yn <- .as_yesno(v)
    isTRUE(yn)
  }, logical(1L))

  long$diagnostik_bereich <- dplyr::case_when(
    grepl("morph", long$diagnostik_bereich, ignore.case = TRUE) ~ "Morphologie",
    grepl("immun", long$diagnostik_bereich, ignore.case = TRUE) ~ "Immunph\u00e4notypisierung",
    grepl("zyto|cyto", long$diagnostik_bereich, ignore.case = TRUE) ~ "Zytogenetik",
    grepl("molekular|molecular|genetik", long$diagnostik_bereich, ignore.case = TRUE) ~ "Molekulargenetik",
    TRUE ~ long$diagnostik_bereich
  )
  long[long$positiv, , drop = FALSE]
}

#' Parse the oncoprint free-text mutation column
#'
#' Splits the free-text mutation column into per-alteration rows, classifies
#' each entry via [onc_alteration_type()], and flags genuine mutations for
#' the oncoprint tile plot.
#'
#' Unlike the legacy v5 implementation, this function does not call
#' [shiny::validate()] -- packaged functions must work outside Shiny. If the
#' required source column is missing, the function raises a `cli::cli_abort`
#' so the server layer can wrap the message with `validate(need(...))`.
#'
#' @param df Cohort data frame from [onc_read_cohort()].
#' @param remove_negative Logical; drop `"negativ/kein Nachweis"` entries
#'   (default `TRUE`).
#'
#' @return A data frame with one row per (patient, alteration) pair:
#'   `patient_label`, `diagnose_label`, `alteration`, `alteration_class`,
#'   `alteration_raw`, `oncoprint_mutation`. Empty input returns a 0-row
#'   tibble with this column set.
#'
#' @family parsers
#' @export
onc_parse_oncoprint <- function(df, remove_negative = TRUE) {
  empty <- .empty_alteration_table(include_oncoprint_flag = TRUE)
  if (is.null(df) || nrow(df) == 0L) return(empty)

  result_col <- .find_col(df, c(
    "krankheitsspezifische_hematol_resultate",
    "krankheitsspezifische_haematol_resultate",
    "hematol_resultate", "haematol_resultate",
    "resultate", "mutation", "mutationen"
  ))
  diag_col <- .find_col(df, c("diagnose", "diagnose_kategorie", "kodierung",
                              "entitaet", "entitaet_onkozert"))
  patient_col <- .find_col(df, c("name", "patient", "patient_id",
                                 "patienten_id", "id"))

  if (is.null(result_col)) {
    cli::cli_abort(
      "Column {.field krankheitsspezifische_hematol_resultate} not found.",
      call = rlang::caller_env()
    )
  }
  if (is.null(diag_col)) {
    cli::cli_abort("No diagnosis/entity column found.",
                   call = rlang::caller_env())
  }

  .alteration_pipeline(
    df = df, source_col = result_col, diag_col = diag_col,
    patient_col = patient_col, value_label = "alteration_raw",
    remove_negative = remove_negative,
    distinct_extra = "oncoprint_mutation"
  )
}

#' Parse the cytogenetics free-text column
#'
#' Same logic as [onc_parse_oncoprint()] but consumes the separate
#' `zytogenetik`/`karyotyp`/`fish` column. Used by the cytogenetics tab.
#'
#' @inheritParams onc_parse_oncoprint
#'
#' @return Data frame with `patient_label`, `diagnose_label`, `alteration`,
#'   `alteration_class`, `zytogenetik_raw`. Empty input returns a 0-row
#'   tibble.
#'
#' @family parsers
#' @export
onc_parse_cytogenetics <- function(df, remove_negative = TRUE) {
  empty <- .empty_alteration_table(include_oncoprint_flag = FALSE,
                                   raw_name = "zytogenetik_raw")
  if (is.null(df) || nrow(df) == 0L) return(empty)

  cyto_col <- .find_col(df, c("zytogenetik", "cytogenetik", "cytogenetics",
                              "karyotyp", "fish", "chromosomenanalyse"))
  diag_col <- .find_col(df, c("diagnose", "diagnose_kategorie", "kodierung",
                              "entitaet", "entitaet_onkozert"))
  patient_col <- .find_col(df, c("name", "patient", "patient_id",
                                 "patienten_id", "id"))

  if (is.null(cyto_col)) {
    cli::cli_abort("Column {.field zytogenetik} not found.",
                   call = rlang::caller_env())
  }
  if (is.null(diag_col)) {
    cli::cli_abort("No diagnosis/entity column found.",
                   call = rlang::caller_env())
  }

  .alteration_pipeline(
    df = df, source_col = cyto_col, diag_col = diag_col,
    patient_col = patient_col, value_label = "zytogenetik_raw",
    remove_negative = remove_negative,
    distinct_extra = NULL
  )
}

# Shared body of the oncoprint + cytogenetics pipelines.
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
  # "t(11;14)") are part of the cytogenetic notation, not a separator.
  tmp$raw_full <- stringr::str_replace_all(tmp$raw_full, "\\n|\\r|/", ",")
  tmp <- tidyr::separate_rows(tmp, "raw_full", sep = ",")
  tmp$raw_full <- trimws(tmp$raw_full)
  tmp$alteration <- onc_normalize_alteration(tmp$raw_full)
  tmp$alteration_class <- onc_alteration_type(tmp$raw_full)
  if (!is.null(distinct_extra)) {
    tmp$oncoprint_mutation <- onc_is_mutation(tmp$alteration_class)
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

# ---- Empty contracts ------------------------------------------------------
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
  out
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
  out
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

#' @keywords internal
.attach_year_month <- function(blocks, date_col_name) {
  d <- blocks[[date_col_name]]
  parsed_date <- suppressWarnings(lubridate::as_date(d))
  year_from_date <- suppressWarnings(lubridate::year(parsed_date))
  year_from_text <- suppressWarnings(
    as.integer(sub(".*(20[0-9]{2}).*", "\\1", d))
  )
  bad_year <- is.na(year_from_text) | year_from_text < 2000 |
    year_from_text > 2100
  year_from_text[bad_year] <- NA_integer_
  blocks$jahr <- ifelse(!is.na(year_from_date), year_from_date, year_from_text)

  month_from_date <- ifelse(
    !is.na(parsed_date), format(parsed_date, "%Y-%m"), NA_character_
  )
  month_from_text <- ifelse(
    !is.na(blocks$jahr),
    paste0(blocks$jahr, "-",
           sprintf("%02d", suppressWarnings(as.integer(substr(d, 4, 5))))),
    NA_character_
  )
  blocks$monat_sort <- ifelse(
    !is.na(month_from_date), month_from_date, month_from_text
  )
  blocks
}
