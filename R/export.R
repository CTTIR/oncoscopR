#' Register the bundled Inter font for live display and exports
#'
#' Registers the OFL Inter weights shipped in `inst/fonts/` with
#' \pkg{systemfonts} so the same `family = "Inter"` renders in the live
#' \pkg{ragg} Shiny device, in 600-dpi PNG exports (`ragg::agg_png`) and in
#' vector PDF exports (`grDevices::cairo_pdf`). Safe to call repeatedly. If the
#' fonts or \pkg{systemfonts} are unavailable it does nothing and figures fall
#' back to the default device font (see [zhn_theme()]); it never errors.
#'
#' @return Invisibly, `TRUE` if Inter was registered, otherwise `FALSE`.
#'
#' @family figures
#' @export
zhn_register_fonts <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  reg <- system.file("fonts", "Inter-Regular.ttf", package = "zhncommandR")
  bold <- system.file("fonts", "Inter-SemiBold.ttf", package = "zhncommandR")
  if (!nzchar(reg) || !file.exists(reg)) {
    return(invisible(FALSE))
  }
  ok <- tryCatch({
    systemfonts::register_font(
      name  = "Inter",
      plain = reg,
      bold  = if (nzchar(bold) && file.exists(bold)) bold else reg
    )
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}

#' Save a zhncommandR figure to PNG (600 dpi) or PDF (vector)
#'
#' One save helper for every figure so resolution, device and font rules live
#' in one place. PNG uses `ragg::agg_png` (sharp text, true 600 dpi, reliable
#' transparency and font rendering); PDF uses `grDevices::cairo_pdf` (vector
#' output that embeds Inter — base `pdf()` would not). The bundled Inter font is
#' registered first so exported text matches the screen.
#'
#' @param plot A [ggplot2::ggplot] (or other printable plot) object.
#' @param file Output path; written as-is.
#' @param format One of `"png"` or `"pdf"`.
#' @param width,height Dimensions in inches.
#' @param transparent Logical; transparent background if `TRUE`, else white.
#'
#' @return The `file` path, invisibly.
#'
#' @family figures
#' @export
#' @examples
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
#'   ggplot2::geom_bar(fill = zhn_pal$accent) + zhn_theme()
#' tmp <- tempfile(fileext = ".png")
#' zhn_save_plot(p, tmp, format = "png", width = 6, height = 4)
zhn_save_plot <- function(plot, file, format = c("png", "pdf"),
                          width = 8, height = 5, transparent = FALSE) {
  format <- match.arg(format)
  zhn_register_fonts()
  bg <- if (isTRUE(transparent)) "transparent" else "white"
  if (format == "png") {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg ragg} is required for 600-dpi PNG export.")
    }
    ggplot2::ggsave(file, plot = plot, device = ragg::agg_png,
                    width = width, height = height, units = "in",
                    dpi = 600, bg = bg)
  } else {
    ggplot2::ggsave(file, plot = plot, device = grDevices::cairo_pdf,
                    width = width, height = height, units = "in", bg = bg)
  }
  invisible(file)
}

#' Slugify a figure title into a safe file basename
#'
#' Lower-cases, transliterates German umlauts (ae/oe/ue/ss), replaces any run of
#' non-alphanumeric characters with a single underscore and trims underscores.
#'
#' @param x A character title.
#' @param fallback Basename to use when `x` is empty after slugifying.
#' @return Length-1 character slug.
#' @keywords internal
.slugify <- function(x, fallback = "figure") {
  if (is.null(x) || !nzchar(trimws(x))) return(fallback)
  s <- tolower(trimws(x))
  # Transliterate German umlauts. Build the characters from code points so the
  # source stays pure ASCII (CRAN portability): U+00E4 a, U+00F6 o, U+00FC u,
  # U+00DF ss.
  umlaut <- c(ae = 0xe4L, oe = 0xf6L, ue = 0xfcL, ss = 0xdfL)
  for (i in seq_along(umlaut)) {
    s <- gsub(intToUtf8(umlaut[[i]]), names(umlaut)[i], s, fixed = TRUE)
  }
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  if (!nzchar(s)) fallback else s
}
