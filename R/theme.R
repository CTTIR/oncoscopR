#' zhncommandR figure theme (Hugo Coder look)
#'
#' A single ggplot2 theme used by every `zhn_plot_*()` function so the figure
#' look is identical everywhere and a restyle is a one-line edit. Minimal panel
#' (light major gridlines only, no minor grid, no panel border), Inter type, and
#' generous margins.
#'
#' @param base_size Base font size in points.
#' @param transparent If `TRUE`, the panel and plot backgrounds are fully
#'   transparent (`fill = NA`); otherwise white. Driven by the app's
#'   transparency toggle so live display and exports match.
#'
#' @return A [ggplot2::theme] object (add it to a plot with `+`).
#'
#' @family figures
#' @export
#' @examples
#' # Returns a ggplot2 theme object; add it to any plot with `+`.
#' th <- zhn_theme()
#' th_transparent <- zhn_theme(base_size = 13, transparent = TRUE)
zhn_theme <- function(base_size = 11, transparent = FALSE) {
  bg <- if (isTRUE(transparent)) NA else "white"
  fam <- .zhn_family()
  ggplot2::theme_minimal(base_size = base_size, base_family = fam) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = bg, colour = NA),
      legend.background = ggplot2::element_rect(fill = bg, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3,
                                               colour = zhn_pal$grid),
      panel.border     = ggplot2::element_blank(),
      plot.title   = ggplot2::element_text(face = "bold", size = base_size + 3,
                                           family = fam),
      axis.title   = ggplot2::element_text(size = base_size, family = fam),
      axis.text    = ggplot2::element_text(size = base_size - 1, family = fam),
      legend.text  = ggplot2::element_text(size = base_size - 1, family = fam),
      legend.title = ggplot2::element_text(size = base_size, family = fam),
      plot.margin  = ggplot2::margin(12, 16, 12, 12)
    )
}

#' zhncommandR figure palette
#'
#' Single source of truth for the figure colours. Hugo Coder blue is the
#' data-ink accent (suite purple stays the app chrome accent); revert the
#' figures to suite purple by changing `accent` to `"#5E2C8E"` here.
#'
#' @format A named list with `accent`, `accent_light`, `accent_dark`, `grid`.
#' @family figures
#' @export
zhn_pal <- list(
  accent       = "#1565C0",
  accent_light = "#5E92F3",
  accent_dark  = "#003c8f",
  grid         = "grey85"
)

#' Resolve the figure font family, degrading gracefully
#'
#' Returns `"Inter"` when the bundled Inter font is registered/available,
#' otherwise `""` (ggplot2's default device family) so figures never warn or
#' fail when Inter cannot be loaded.
#'
#' @return Length-1 character font family.
#' @keywords internal
.zhn_family <- function() {
  ok <- tryCatch(
    requireNamespace("systemfonts", quietly = TRUE) &&
      "Inter" %in% systemfonts::registry_fonts()$family,
    error = function(e) FALSE
  )
  if (isTRUE(ok)) "Inter" else ""
}
