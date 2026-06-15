#' Coerce documentation variants to logical Yes/No/NA
#'
#' Accepts the documentation variants commonly seen in haematology tumour
#' centre spreadsheets — German and English, numeric and boolean, and the
#' usual missing-value sentinels — and returns a strict logical.
#'
#' @param x A vector. Logical input is returned unchanged.
#'
#' @return Logical vector the same length as `x`. `TRUE` for affirmative,
#'   `FALSE` for negative, `NA` otherwise.
#'
#' @family helpers
#' @keywords internal
.as_yesno <- function(x) {
  if (is.logical(x)) return(x)
  x_chr <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(x_chr))
  yes_tokens <- c("1", "ja", "yes", "y", "true", "wahr", "x")
  no_tokens  <- c("0", "nein", "no", "n", "false", "falsch")
  out[x_chr %in% yes_tokens] <- TRUE
  out[x_chr %in% no_tokens]  <- FALSE
  out
}

#' Robust event coding for Kaplan-Meier
#'
#' Coerces a vector to the strict `0`/`1` event coding required by
#' [survival::survfit()]. Dates (and POSIXct) map to `1` when present and `0`
#' when missing, useful for free-text recurrence-date columns. Strings are
#' parsed against an explicit, anchored token list — no prefix matching.
#'
#' This function deliberately differs from the legacy v5 implementation in
#' one place: `"10 months"` now returns `NA`, not `1`. The original used
#' `grepl("^1", x)`, which incorrectly classified any string starting with
#' `1` as an event.
#'
#' @param x A vector — logical, numeric, character, Date or POSIXct.
#' @param mode `"auto"` (default) treats only explicit tokens as events.
#'   `"date_event"` additionally treats any non-empty, non-missing value as
#'   an event (used for date-style columns such as recurrence date, where
#'   presence of a date *is* the event).
#'
#' @return Numeric vector of `0`/`1`/`NA`, the same length as `x`.
#'
#' @family helpers
#' @keywords internal
.as_event01 <- function(x, mode = c("auto", "date_event", "death_event")) {
  mode <- match.arg(mode)

  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(ifelse(is.na(x), 0, 1))
  }

  x_chr <- trimws(as.character(x))
  x_low <- tolower(x_chr)

  empty_tokens <- c("", "na", "n/a", "?", "unbekannt", "nicht bekannt")
  yes_tokens   <- c("1", "ja", "j", "yes", "y", "true", "wahr", "x",
                    "tod", "verstorben")
  no_tokens    <- c("0", "nein", "n", "no", "false", "falsch", "lebt", "alive")

  out <- rep(NA_real_, length(x_low))
  is_empty <- is.na(x) | x_low %in% empty_tokens
  out[x_low %in% yes_tokens] <- 1
  out[x_low %in% no_tokens]  <- 0
  out[is_empty] <- NA_real_

  if (mode == "date_event") {
    out[is.na(out) & !is_empty] <- 1
    out[is_empty] <- 0
  }

  out
}

#' Robust date coercion
#'
#' Pass-through for `Date` and POSIXct; everything else goes through
#' `as.Date(x)` with warnings suppressed.
#'
#' @param x A vector.
#'
#' @return A `Date` vector.
#'
#' @family helpers
#' @keywords internal
.safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  suppressWarnings(as.Date(x))
}

#' Add a `behandlungsjahr` (treatment year) column
#'
#' Derives the treatment year from the first available date column among
#' `erstvorstellung`, `erstdiagnose`, `therapie_beginn`, `tumorkonferenz_datum`,
#' `last_follow_up`. If none of these exist, the column is filled with `NA`.
#'
#' If multiple matching columns exist (e.g. duplicate `erstdiagnose` columns
#' that `janitor::clean_names()` renames to `erstdiagnose` and
#' `erstdiagnose_2`), the *first* matching column wins.
#'
#' @param df A data frame.
#'
#' @return The input `df` with an added integer column `behandlungsjahr`.
#'
#' @family helpers
#' @keywords internal
.add_year <- function(df) {
  date_col <- .find_col(df, c(
    "erstvorstellung", "erstdiagnose", "therapie_beginn",
    "tumorkonferenz_datum", "last_follow_up"
  ))
  if (!is.null(date_col)) {
    df$behandlungsjahr <- as.integer(
      lubridate::year(.safe_date(df[[date_col]]))
    )
  } else {
    df$behandlungsjahr <- NA_integer_
  }
  df
}
