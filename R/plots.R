#' Figure: top diagnoses / codings (horizontal bar)
#'
#' Counts the most frequent values of a diagnosis/coding column and draws a
#' horizontal bar chart in the Hugo Coder look. Title-less by design — the name
#' lives in the card header and the export filename, not in the plot.
#'
#' @param data A data frame (typically the filtered cohort).
#' @param col Name of the diagnosis/coding column to count.
#' @param n Number of top categories to show.
#' @param transparent Passed to [zhn_theme()].
#' @param ylab Count-axis label.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @family figures
#' @export
zhn_plot_diagnoses <- function(data, col, n = 20, transparent = FALSE,
                               ylab = "Anzahl") {
  data |>
    dplyr::count(.data[[col]], sort = TRUE) |>
    dplyr::slice_head(n = n) |>
    ggplot2::ggplot(ggplot2::aes(
      x = stats::reorder(.data[[col]], .data$n), y = .data$n
    )) +
    ggplot2::geom_col(fill = zhn_pal$accent) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = ylab) +
    zhn_theme(transparent = transparent)
}

#' Figure: cases per treatment year (vertical bar)
#'
#' @param data A data frame with a treatment-year column.
#' @param year_col Name of the year column.
#' @param transparent Passed to [zhn_theme()].
#' @param xlab,ylab Axis labels.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @family figures
#' @export
zhn_plot_cases_by_year <- function(data, year_col = "behandlungsjahr",
                                   transparent = FALSE, xlab = "Jahr",
                                   ylab = "Anzahl") {
  data |>
    dplyr::filter(!is.na(.data[[year_col]])) |>
    dplyr::count(.data[[year_col]]) |>
    ggplot2::ggplot(ggplot2::aes(x = factor(.data[[year_col]]), y = .data$n)) +
    ggplot2::geom_col(fill = zhn_pal$accent) +
    ggplot2::labs(x = xlab, y = ylab) +
    zhn_theme(transparent = transparent)
}

#' Figure: Kaplan-Meier survival curve
#'
#' Draws a themed Kaplan-Meier curve from a fitted [survival::survfit] object.
#' Unlike the other figures the KM plot keeps an in-plot title (user-editable in
#' the app). A single overall curve uses the Hugo Coder blue accent; multiple
#' strata use a blue-anchored, greyscale-safe viridis ("mako") ramp.
#'
#' @param fit A [survival::survfit] object (single curve or with strata).
#' @param title Optional plot title; omitted when `NULL`/empty.
#' @param xlab,ylab Axis labels.
#' @param transparent Passed to [zhn_theme()].
#' @param show_ci Draw the confidence-interval ribbon.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @family figures
#' @export
zhn_plot_km <- function(fit, title = NULL, xlab = "Monate",
                        ylab = "Wahrscheinlichkeit", transparent = FALSE,
                        show_ci = TRUE) {
  s <- summary(fit)
  dat <- data.frame(
    time   = s$time,
    surv   = s$surv,
    upper  = if (!is.null(s$upper)) s$upper else s$surv,
    lower  = if (!is.null(s$lower)) s$lower else s$surv,
    strata = if (!is.null(s$strata)) as.character(s$strata) else "Gesamt",
    stringsAsFactors = FALSE
  )
  multi <- length(unique(dat$strata)) > 1L
  p <- ggplot2::ggplot(dat, ggplot2::aes(
    x = .data$time, y = .data$surv,
    colour = .data$strata, fill = .data$strata
  ))
  if (isTRUE(show_ci)) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      alpha = 0.15, colour = NA
    )
  }
  p <- p +
    ggplot2::geom_step(linewidth = 0.7) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(
      x = xlab, y = ylab, colour = NULL, fill = NULL,
      title = if (!is.null(title) && nzchar(title)) title else NULL
    ) +
    zhn_theme(transparent = transparent)
  if (multi) {
    p <- p +
      ggplot2::scale_colour_viridis_d(option = "mako", begin = 0.15, end = 0.8) +
      ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.15, end = 0.8)
  } else {
    p <- p +
      ggplot2::scale_colour_manual(values = zhn_pal$accent, guide = "none") +
      ggplot2::scale_fill_manual(values = zhn_pal$accent, guide = "none")
  }
  p
}
