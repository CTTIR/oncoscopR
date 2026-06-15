#' Normalise a free-text alteration label
#'
#' Collapses whitespace and strips trailing "Mutation"/"Mut" suffixes used by
#' clinicians documenting variants in free text.
#'
#' @param x Character vector.
#'
#' @return Character vector the same length as `x`.
#'
#' @family alteration
#' @export
#' @examples
#' onc_normalize_alteration(c("TP53 Mutation", "  NRAS  Mut", "del(17p)"))
onc_normalize_alteration <- function(x) {
  x <- trimws(as.character(x))
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_replace_all(
    x, stringr::regex("Mutation$|Mut$", ignore_case = TRUE), ""
  )
  trimws(x)
}

#' Classify a free-text alteration string
#'
#' Domain rules for separating true mutations/variants from structural and
#' cytogenetic findings, and from negative/missing entries. The categories
#' returned drive the oncoprint vs. cytogenetics-table split in the dashboard.
#'
#' The classification regex is clinical-domain knowledge — do not "improve"
#' it without a corresponding update to the test fixtures.
#'
#' @param x Character vector of free-text alteration strings.
#'
#' @return Character vector the same length as `x`, drawn from:
#'   `"Nicht verwertbar/NA"`,
#'   `"negativ/kein Nachweis"`,
#'   `"Strukturell/Zytogenetik: Deletion/Loss"`,
#'   `"Strukturell/Zytogenetik: Zugewinn/Amplifikation"`,
#'   `"Strukturell/Zytogenetik: Translokation/Rearrangement/Bruch"`,
#'   `"Strukturell/Zytogenetik: Komplexer Karyotyp"`,
#'   `"Mutation/Variante"`.
#'
#' @family alteration
#' @export
#' @examples
#' onc_alteration_type(c(
#'   "TP53 Mutation", "del(17p)", "Trisomie 12",
#'   "t(11;14)", "komplexer Karyotyp", "negativ", NA
#' ))
onc_alteration_type <- function(x) {
  xl <- tolower(as.character(x))
  out <- rep(NA_character_, length(xl))

  is_na <- is.na(x) | trimws(xl) == "" |
    xl %in% c("na", "n.a.", "n/a", "nan", "null")
  is_neg <- !is_na & stringr::str_detect(
    xl, "negativ|kein\\b|keine\\b|ohne\\b|nicht nachweis|kein nachweis|wt\\b|wildtyp|wild type"
  )
  is_del <- !is_na & !is_neg & stringr::str_detect(
    xl, "del\\b|deletion|verlust|loss|monosomie|minus|\\-"
  )
  is_gain <- !is_na & !is_neg & !is_del & stringr::str_detect(
    xl, "gain|zugewinn|zugewin|amplifikation|amplification|amp\\b|trisomie|plus|\\+"
  )
  is_trans <- !is_na & !is_neg & !is_del & !is_gain & stringr::str_detect(
    xl, "translokation|translocation|translation|rearrangement|bruch|break|fusion|t\\("
  )
  is_complex <- !is_na & !is_neg & !is_del & !is_gain & !is_trans &
    stringr::str_detect(xl, "karyotyp|komplex|complex")

  out[is_na] <- "Nicht verwertbar/NA"
  out[is_neg] <- "negativ/kein Nachweis"
  out[is_del] <- "Strukturell/Zytogenetik: Deletion/Loss"
  out[is_gain] <- "Strukturell/Zytogenetik: Zugewinn/Amplifikation"
  out[is_trans] <- "Strukturell/Zytogenetik: Translokation/Rearrangement/Bruch"
  out[is_complex] <- "Strukturell/Zytogenetik: Komplexer Karyotyp"
  out[is.na(out)] <- "Mutation/Variante"
  out
}

#' Logical: should this alteration class appear in the oncoprint?
#'
#' Convenience predicate. Only true mutations/variants populate the oncoprint
#' tile plot; structural/cytogenetic findings are reported in their own
#' tabular and cytogenetics-tab views.
#'
#' @param alteration_class Character vector as returned by
#'   [onc_alteration_type()].
#'
#' @return Logical vector the same length as `alteration_class`.
#'
#' @family alteration
#' @export
#' @examples
#' onc_is_mutation(onc_alteration_type(c("TP53", "del(17p)")))
onc_is_mutation <- function(alteration_class) {
  alteration_class == "Mutation/Variante"
}
