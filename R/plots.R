#' Figure: top diagnoses / codings (horizontal bar)
#'
#' Counts the most frequent values of a diagnosis/coding column and draws a
#' horizontal bar chart in the Hugo Coder look. Title-less by design -- the name
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

#' Build the modelling frame for a Kaplan-Meier analysis
#'
#' Coerces the time/event (and optional group) columns and applies the survival
#' guards. Raises a `cli` condition (never `shiny::validate`) on unusable input;
#' the server turns the condition into a `validate(need())` message.
#'
#' @inheritParams zhn_plot_km
#' @return A list with `df` (columns `time`, `event`, optional `grp`) and
#'   `n_groups`.
#' @keywords internal
.km_frame <- function(data, time_col, event_col, group_col = NULL) {
  time <- suppressWarnings(as.numeric(data[[time_col]]))
  if (all(is.na(time))) {
    cli::cli_abort("Zeitspalte {.val {time_col}} ist nicht numerisch.")
  }
  event <- .as_event01(data[[event_col]], mode = "auto")
  ok <- !is.na(time) & !is.na(event) & time >= 0 & event %in% c(0, 1)
  if (sum(ok) < 2L) {
    cli::cli_abort(paste0(
      "Gew", intToUtf8(0xe4L), "hlte Ereignis-Spalte enth", intToUtf8(0xe4L),
      "lt keine auswertbaren 0/1-Ereignisse."
    ))
  }
  df <- data.frame(time = time[ok], event = as.integer(event[ok]))
  has_group <- !is.null(group_col) && nzchar(group_col) &&
    group_col %in% names(data)
  if (has_group) {
    grp <- factor(data[[group_col]][ok])
    keep <- !is.na(grp)
    df <- df[keep, , drop = FALSE]
    df$grp <- droplevels(grp[keep])
    if (nlevels(df$grp) < 2L) df$grp <- NULL
  }
  list(df = df, n_groups = if (is.null(df$grp)) 1L else nlevels(df$grp))
}

#' Pairwise log-rank comparisons with Benjamini-Hochberg correction
#'
#' For a grouping with more than two levels, computes a log-rank test for every
#' pair of groups and adjusts the raw p-values with the BH method.
#'
#' @inheritParams zhn_plot_km
#' @return A data frame with `Gruppe_1`, `Gruppe_2`, `p_roh`, `p_BH`.
#' @family figures
#' @export
zhn_km_pairwise <- function(data, time_col, event_col, group_col) {
  fr <- .km_frame(data, time_col, event_col, group_col)
  df <- fr$df
  if (is.null(df$grp) || nlevels(df$grp) < 3L) {
    cli::cli_abort(paste0(
      "Paarweise Vergleiche ben", intToUtf8(0xf6L), "tigen mehr als zwei Gruppen."
    ))
  }
  combs <- utils::combn(levels(df$grp), 2L, simplify = FALSE)
  rows <- lapply(combs, function(pr) {
    sub <- df[df$grp %in% pr, , drop = FALSE]
    sub$grp <- droplevels(sub$grp)
    sd <- survival::survdiff(survival::Surv(time, event) ~ grp, data = sub)
    pv <- stats::pchisq(sd$chisq, df = length(sd$n) - 1L, lower.tail = FALSE)
    data.frame(Gruppe_1 = pr[1], Gruppe_2 = pr[2], p_roh = pv)
  })
  out <- dplyr::bind_rows(rows)
  out$p_BH <- stats::p.adjust(out$p_roh, method = "BH")
  out
}

#' Publication-ready Kaplan-Meier plot (ggsurvfit)
#'
#' Builds a themed Kaplan-Meier figure with \pkg{ggsurvfit}. Every publication
#' element is an argument; the same call drives the live app and the PNG/PDF
#' export. With a risk table the return value is the aligned plot+table
#' composite ([ggsurvfit::ggsurvfit_build()] output) so [zhn_save_plot()] writes
#' the full figure. This is the one figure that keeps an (editable) title.
#'
#' @param data A data frame / cohort with the time and event columns.
#' @param time_col,event_col Column names (character) for follow-up time and
#'   event; the event is parsed to 0/1 via the package's hardened coercion.
#' @param group_col Optional grouping column (character), or `NULL` for one
#'   overall curve.
#' @param conf_int Logical; show confidence-interval ribbons.
#' @param conf_level CI level (e.g. 0.90/0.95/0.99).
#' @param conf_type CI method: `"log-log"` (default), `"log"`, or `"plain"`.
#' @param risktable Logical; show the numbers-at-risk table below the plot.
#' @param risktable_stats Which rows: any of `"n.risk"`, `"n.censor"`,
#'   `"n.event"`.
#' @param censor_marks Logical; show censoring tick marks.
#' @param show_pvalue Logical; annotate the log-rank p-value (>= 2 groups).
#' @param show_hr Logical; annotate the Cox HR with 95% CI (exactly 2 groups),
#'   always with a `cox.zph` proportional-hazards check and a caution note when
#'   violated.
#' @param show_median Logical; draw reference lines at the median survival.
#' @param y_scale `"survival"` (0-1), `"percent"` (0-100), or `"cuminc"`
#'   (1 - S).
#' @param x_break Numeric x-axis break interval, or `NULL` for automatic.
#' @param x_max Numeric x-axis truncation, or `NULL` for full range.
#' @param legend_pos `"top"`, `"right"`, `"bottom"`, or `"none"`.
#' @param title,subtitle,caption Character or `NULL`.
#' @param transparent Logical; transparent background for display/export.
#'
#' @return A \pkg{ggsurvfit}/ggplot object; a patchwork composite when
#'   `risktable = TRUE`. Carries an `"n_groups"` attribute.
#'
#' @family figures
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("ggsurvfit", quietly = TRUE)) {
#'   co <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
#'   zhn_plot_km(co, "os", "death_event")                 # default single curve
#'   zhn_plot_km(co, "os", "death_event", group_col = "geschlecht",
#'               show_pvalue = TRUE)                       # grouped + log-rank
#' }
#' }
zhn_plot_km <- function(data, time_col, event_col, group_col = NULL,
                        conf_int = TRUE, conf_level = 0.95,
                        conf_type = "log-log",
                        risktable = TRUE, risktable_stats = "n.risk",
                        censor_marks = TRUE, show_pvalue = TRUE,
                        show_hr = FALSE, show_median = FALSE,
                        y_scale = c("survival", "percent", "cuminc"),
                        x_break = NULL, x_max = NULL, legend_pos = "top",
                        title = NULL, subtitle = NULL, caption = NULL,
                        transparent = FALSE) {
  y_scale <- match.arg(y_scale)
  fr <- .km_frame(data, time_col, event_col, group_col)
  df <- fr$df
  n_groups <- fr$n_groups
  has_group <- n_groups >= 2L

  fit <- if (has_group) {
    ggsurvfit::survfit2(survival::Surv(time, event) ~ grp, data = df,
                        conf.int = conf_level, conf.type = conf_type)
  } else {
    ggsurvfit::survfit2(survival::Surv(time, event) ~ 1, data = df,
                        conf.int = conf_level, conf.type = conf_type)
  }

  type_arg <- if (y_scale == "cuminc") "risk" else "survival"
  # German labels with umlauts built from code points (ASCII source for CRAN).
  ue_surv <- paste0(intToUtf8(0xdcL), "berlebenswahrscheinlichkeit")
  ylab <- switch(y_scale,
    survival = ue_surv,
    percent  = paste0(ue_surv, " (%)"),
    cuminc   = "Kumulative Inzidenz"
  )

  p <- ggsurvfit::ggsurvfit(fit, type = type_arg, linewidth = 0.8)

  # x scale FIRST so the risk table inherits identical breaks/limits (sec 1.5).
  maxt <- max(df$time, na.rm = TRUE)
  upper <- if (!is.null(x_max)) x_max else maxt
  brks <- if (!is.null(x_break)) seq(0, upper, by = x_break) else ggplot2::waiver()
  lims <- if (!is.null(x_max)) c(0, x_max) else NULL
  p <- p + ggplot2::scale_x_continuous(breaks = brks, limits = lims)

  if (isTRUE(conf_int)) p <- p + ggsurvfit::add_confidence_interval(alpha = 0.18)
  if (isTRUE(censor_marks)) p <- p + ggsurvfit::add_censor_mark()
  if (isTRUE(show_median)) {
    p <- p + ggsurvfit::add_quantile(y_value = 0.5, linetype = "dashed",
                                     linewidth = 0.4, colour = "grey50")
  }
  if (isTRUE(show_pvalue) && n_groups >= 2L) {
    p <- p + ggsurvfit::add_pvalue("annotation", size = 3.4)
  }
  if (isTRUE(risktable)) {
    stats <- intersect(risktable_stats, c("n.risk", "n.censor", "n.event"))
    if (!length(stats)) stats <- "n.risk"
    p <- p + ggsurvfit::add_risktable(risktable_stats = stats,
                                      size = 3.2)
  }

  # Colours: single curve = Hugo Coder accent; strata = blue-anchored mako.
  if (has_group) {
    p <- p +
      ggplot2::scale_colour_viridis_d(option = "mako", begin = 0.12, end = 0.78) +
      ggplot2::scale_fill_viridis_d(option = "mako", begin = 0.12, end = 0.78)
  } else {
    p <- p +
      ggplot2::scale_colour_manual(values = zhn_pal$accent) +
      ggplot2::scale_fill_manual(values = zhn_pal$accent)
  }

  if (y_scale == "percent") {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, 1),
                                         labels = scales::label_percent())
  }

  # Cox HR + mandatory proportional-hazards check (exactly 2 groups).
  if (isTRUE(show_hr) && n_groups == 2L) {
    cox <- survival::coxph(survival::Surv(time, event) ~ grp, data = df)
    ci <- summary(cox)$conf.int[1, ]
    hr_txt <- sprintf("HR = %.2f (95%% CI %.2f-%.2f)", ci[1], ci[3], ci[4])
    subtitle <- paste(c(subtitle, hr_txt), collapse = "\n")
    zph <- tryCatch(survival::cox.zph(cox), error = function(e) NULL)
    ph_p <- if (!is.null(zph)) zph$table["GLOBAL", "p"] else NA_real_
    if (is.finite(ph_p) && ph_p < 0.05) {
      note <- sprintf(
        "PH-Annahme evtl. verletzt (cox.zph p = %.3f); HR mit Vorsicht.", ph_p
      )
      caption <- paste(c(caption, note), collapse = "\n")
      cli::cli_inform(note)
    }
  }

  p <- p +
    zhn_theme(transparent = transparent) +
    ggplot2::theme(legend.position = legend_pos) +
    ggplot2::labs(
      x = "Zeit (Monate)", y = ylab, colour = NULL, fill = NULL,
      title    = if (!is.null(title) && nzchar(title)) title else NULL,
      subtitle = if (!is.null(subtitle) && nzchar(subtitle)) subtitle else NULL,
      caption  = if (!is.null(caption) && nzchar(caption)) caption else NULL
    )

  out <- if (isTRUE(risktable)) ggsurvfit::ggsurvfit_build(p) else p
  attr(out, "n_groups") <- n_groups
  out
}
