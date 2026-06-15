#' Find the first candidate column present in a data frame
#'
#' Searches a vector of candidate column names and returns the first one that
#' exists in `df`. Used throughout the package to tolerate the assorted column
#' naming conventions of tumour-documentation spreadsheets.
#'
#' @param df A data frame.
#' @param candidates Character vector of candidate column names, in priority
#'   order.
#'
#' @return Length-1 character scalar with the matched column name, or `NULL`
#'   if no candidate matched. Never returns `character(0)`.
#'
#' @family helpers
#' @export
#' @examples
#' df <- data.frame(diagnose = "AML", kodierung = "C92.0")
#' .find_col_example <- function() NULL
#' # find_col_internal is internal; users typically call onc_read_* instead
.find_col <- function(df, candidates) {
  hits <- candidates[candidates %in% names(df)]
  if (length(hits) == 0L) return(NULL)
  hits[[1L]]
}

#' Row-wise first non-empty value across columns
#'
#' For each row of `data`, returns the first non-NA, non-empty value among the
#' supplied column names, scanning in order. Used to coalesce duplicate-named
#' patient identifier columns (e.g. `patient`, `patient_2`).
#'
#' @param data A data frame.
#' @param cols Character vector of column names to scan in priority order.
#'   Missing columns are silently skipped.
#'
#' @return Character vector of length `nrow(data)`. `NA_character_` where no
#'   column had a non-empty value.
#'
#' @family helpers
#' @keywords internal
.first_nonempty_col <- function(data, cols) {
  if (length(cols) == 0L || nrow(data) == 0L) {
    return(rep(NA_character_, nrow(data)))
  }
  out <- rep(NA_character_, nrow(data))
  for (col in cols) {
    if (!col %in% names(data)) next
    val <- trimws(as.character(data[[col]]))
    val[is.na(val) | val == ""] <- NA_character_
    idx <- is.na(out) & !is.na(val)
    out[idx] <- val[idx]
  }
  out
}

#' Count distinct non-empty values in a vector
#'
#' Trims whitespace, drops `NA` and empty strings, then returns the number of
#' distinct remaining values.
#'
#' @param x A vector (typically character).
#'
#' @return Integer scalar.
#'
#' @family helpers
#' @keywords internal
.n_distinct_nonempty <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
  length(unique(x_chr))
}
