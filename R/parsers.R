#' Prepare OPS-8-544 therapy blocks for downstream analysis
#'
#' `r lifecycle::badge("experimental")`
#'
#' Reshapes the raw therapy sheet (as returned by [zhn_read_therapy()]) into
#' the per-block tibble the dashboard counts. Counts every numeric
#' OPS-8-544 entry > 0 as a positive block; falls back to deduplicating
#' Patient/Datum/Zyklus/Protokoll combinations when no OPS column exists.
#'
#' @param df Data frame returned by [zhn_read_therapy()].
#'
#' @return A [therapy_blocks][new_therapy_blocks] S3 object (inheriting from
#'   `data.frame`) with at minimum: `therapieprotokoll`, `diagnose`,
#'   `patient`, `datum`, `zyklus`, `jahr`, `monat_sort`. Empty input returns
#'   a 0-row object with this column set; the `patient_cols_used` attribute
#'   records which raw columns were coalesced into `patient`.
#'
#' @family parsers
#' @export
zhn_prepare_therapy_blocks <- function(df) {
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
  patient_cols_used <- paste(
    c(patient_name_cols, patient_id_cols), collapse = ", "
  )

  # Respect Date / POSIXct input; only coerce to character if it isn't already.
  if (is.null(date_col) || !(date_col %in% names(blocks))) {
    blocks$datum <- rep(NA_character_, nrow(blocks))
  } else if (inherits(blocks[[date_col]], c("Date", "POSIXct", "POSIXt"))) {
    blocks$.datum_raw <- blocks[[date_col]]
    blocks$datum <- format(blocks[[date_col]], "%Y-%m-%d")
  } else {
    blocks$datum <- as.character(blocks[[date_col]])
  }
  blocks$zyklus <- get_chr(blocks, cycle_col, NA_character_)

  empty_prot <- is.na(blocks$therapieprotokoll) |
    trimws(blocks$therapieprotokoll) == ""
  blocks$therapieprotokoll[empty_prot] <- "Nicht angegeben"
  empty_diag <- is.na(blocks$diagnose) | trimws(blocks$diagnose) == ""
  blocks$diagnose[empty_diag] <- "Nicht angegeben"

  blocks <- .attach_year_month(
    blocks,
    date_col_name = if (".datum_raw" %in% names(blocks)) ".datum_raw" else "datum"
  )
  blocks$.datum_raw <- NULL
  new_therapy_blocks(blocks, patient_cols_used = patient_cols_used)
}

#' Prepare OPS-1-941 complex-diagnostics blocks
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param df Data frame returned by [zhn_read_diagnostics()].
#'
#' @return A [diagnostic_blocks][new_diagnostic_blocks] S3 object with
#'   `patient`, `diagnose`, `datum`, `primaerfall`, `patientenfall`, `jahr`,
#'   `monat_sort`, plus the component columns (Morphologie,
#'   Immunphaenotypisierung, Zytogenetik, Molekulargenetik). Empty input
#'   returns a 0-row object.
#'
#' @family parsers
#' @export
zhn_prepare_diagnostic_blocks <- function(df) {
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
  if (is.null(date_col) || !(date_col %in% names(blocks))) {
    blocks$datum <- rep(NA_character_, nrow(blocks))
  } else if (inherits(blocks[[date_col]], c("Date", "POSIXct", "POSIXt"))) {
    blocks$.datum_raw <- blocks[[date_col]]
    blocks$datum <- format(blocks[[date_col]], "%Y-%m-%d")
  } else {
    blocks$datum <- as.character(blocks[[date_col]])
  }
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

  # Tighter component-column whitelist: only the four canonical OPS-1-941
  # components count. Loose substring matches like `psychoonkologische_diagnostik`
  # are explicitly excluded by anchoring the regex with `^`.
  component_patterns <- c(
    "morph", "immun", "zyto", "cyto", "molekular", "molecular"
  )
  comp_re <- paste0("^(", paste(component_patterns, collapse = "|"), ")")
  candidate_names <- names(blocks)[grepl(comp_re, names(blocks),
                                         ignore.case = TRUE)]
  comp_candidates <- setdiff(candidate_names, c(ops_col, ".diag_event"))
  component_cols <- paste(comp_candidates, collapse = ", ")

  blocks <- .attach_year_month(
    blocks,
    date_col_name = if (".datum_raw" %in% names(blocks)) ".datum_raw" else "datum"
  )
  blocks$.datum_raw <- NULL
  new_diagnostic_blocks(blocks, component_cols = component_cols)
}

#' Pivot diagnostic-block components into long form
#'
#' Detects component columns (Morphologie / Immunphaenotypisierung /
#' Zytogenetik / Molekulargenetik), pivots them long, normalises the display
#' label, and filters to positive entries only.
#'
#' @param blocks Tibble from [zhn_prepare_diagnostic_blocks()].
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

  component_patterns <- c(
    "morph", "immun", "zyto", "cyto", "molekular", "molecular"
  )
  comp_re <- paste0("^(", paste(component_patterns, collapse = "|"), ")")
  comp_cols <- names(blocks)[grepl(comp_re, names(blocks),
                                   ignore.case = TRUE)]
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
    grepl("immun", long$diagnostik_bereich, ignore.case = TRUE) ~ "Immunphaenotypisierung",
    grepl("zyto|cyto", long$diagnostik_bereich, ignore.case = TRUE) ~ "Zytogenetik",
    grepl("molekular|molecular", long$diagnostik_bereich, ignore.case = TRUE) ~ "Molekulargenetik",
    TRUE ~ long$diagnostik_bereich
  )
  long[long$positiv, , drop = FALSE]
}

#' Parse the oncoprint free-text mutation column
#'
#' `r lifecycle::badge("experimental")`
#'
#' Splits the free-text mutation column into per-alteration rows, classifies
#' each entry via [zhn_alteration_type()], and flags genuine mutations for
#' the oncoprint tile plot.
#'
#' Unlike the legacy v5 implementation, this function does not call
#' [shiny::validate()] -- packaged functions must work outside Shiny. If the
#' required source column is missing, the function raises a `cli::cli_abort`
#' so the server layer can wrap the message with `validate(need(...))`.
#'
#' @param df Cohort data frame from [zhn_read_cohort()].
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
zhn_parse_oncoprint <- function(df, remove_negative = TRUE) {
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
#' `r lifecycle::badge("experimental")`
#'
#' Same logic as [zhn_parse_oncoprint()] but consumes the separate
#' `zytogenetik`/`karyotyp`/`fish` column. Used by the cytogenetics tab.
#'
#' @inheritParams zhn_parse_oncoprint
#'
#' @return Data frame with `patient_label`, `diagnose_label`, `alteration`,
#'   `alteration_class`, `zytogenetik_raw`. Empty input returns a 0-row
#'   tibble.
#'
#' @family parsers
#' @export
zhn_parse_cytogenetics <- function(df, remove_negative = TRUE) {
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
