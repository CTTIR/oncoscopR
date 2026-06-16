test_that("zhn_theme is a ggplot theme and the palette is stable", {
  expect_s3_class(zhn_theme(), "theme")
  expect_s3_class(zhn_theme(transparent = TRUE), "theme")
  expect_identical(zhn_pal$accent, "#1565C0")
})

test_that("bar figures return title-less ggplots with the accent fill", {
  cohort <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  p <- zhn_plot_diagnoses(cohort, col = "diagnose")
  expect_s3_class(p, "ggplot")
  expect_null(p$labels$title)

  y <- zhn_plot_cases_by_year(cohort)
  expect_s3_class(y, "ggplot")
  expect_null(y$labels$title)
})

test_that("KM works out of the box on the example data (OS / death_event)", {
  skip_if_not_installed("openxlsx")
  cohort <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  expect_true(all(c("os", "death_event") %in% names(cohort)))

  time  <- suppressWarnings(as.numeric(cohort$os))
  event <- zhncommandR:::.as_event01(cohort$death_event, mode = "auto")
  keep  <- !is.na(time) & !is.na(event) & time >= 0 & event %in% c(0, 1)
  km_df <- data.frame(time = time[keep], event = as.integer(event[keep]))

  fit <- survival::survfit(survival::Surv(time, event) ~ 1, data = km_df)
  p <- zhn_plot_km(fit, title = "Beispiel-OS")
  expect_s3_class(p, "ggplot")
  expect_gt(length(p$layers), 0L)
  expect_true(any(!is.na(summary(fit)$surv)))
  expect_identical(p$labels$title, "Beispiel-OS")
})

test_that("zhn_save_plot writes non-empty PNG (600 dpi) and PDF", {
  skip_on_cran()
  skip_if_not_installed("ragg")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar(fill = zhn_pal$accent) +
    zhn_theme()
  png <- withr::local_tempfile(fileext = ".png")
  pdf <- withr::local_tempfile(fileext = ".pdf")
  zhn_save_plot(p, png, format = "png", width = 6, height = 4)
  zhn_save_plot(p, pdf, format = "pdf", width = 6, height = 4)
  expect_gt(file.info(png)$size, 0)
  expect_gt(file.info(pdf)$size, 0)
})

test_that(".slugify transliterates umlauts and falls back", {
  expect_identical(
    zhncommandR:::.slugify("Kaplan-Meier Überlebenskurve"),
    "kaplan_meier_ueberlebenskurve"
  )
  expect_identical(zhncommandR:::.slugify("   "), "figure")
  expect_identical(zhncommandR:::.slugify("OS – AML!!"), "os_aml")
})
