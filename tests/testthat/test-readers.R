test_that(".check_file_exists aborts cleanly on missing file", {
  expect_error(oncoscopR:::.check_file_exists(NULL), "No file path supplied")
  expect_error(oncoscopR:::.check_file_exists(""), "No file path supplied")
  expect_error(oncoscopR:::.check_file_exists("/no/such/file.xlsx"),
               "File not found")
})

test_that(".resolve_sheet errors clearly when no role match exists", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(Random = data.frame(a = 1)),
    file = tmp
  )
  expect_error(
    oncoscopR:::.resolve_sheet(tmp, "cohort"),
    "No sheet matching role"
  )
})

test_that(".resolve_sheet picks the canonical name first", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Basisdaten = data.frame(name = "a", diagnose = "AML"),
      Faelle     = data.frame(other = 1)
    ),
    file = tmp
  )
  expect_identical(
    oncoscopR:::.resolve_sheet(tmp, "cohort"),
    "Basisdaten"
  )
})

test_that(".resolve_sheet honours explicit sheet override", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(Basisdaten = data.frame(a = 1), Custom = data.frame(b = 1)),
    file = tmp
  )
  expect_identical(
    oncoscopR:::.resolve_sheet(tmp, "cohort", sheet = "Custom"),
    "Custom"
  )
})

test_that(".resolve_sheet falls back to regex for non-canonical names", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(MeineFaelle = data.frame(a = 1)),
    file = tmp
  )
  expect_identical(
    oncoscopR:::.resolve_sheet(tmp, "cohort"),
    "MeineFaelle"
  )
})

test_that(".clean_duplicate_columns drops empty `none*` cols", {
  df <- data.frame(
    name = c("A", "B"),
    none = c(NA, NA),
    none_2 = c("", ""),
    other = c(1, 2),
    stringsAsFactors = FALSE
  )
  out <- oncoscopR:::.clean_duplicate_columns(df, verbose = FALSE)
  expect_identical(names(out), c("name", "other"))
})

test_that(".clean_duplicate_columns warns about surviving _2 suffixed cols", {
  df <- data.frame(
    erstdiagnose   = as.Date(c("2024-01-01", "2024-06-15")),
    erstdiagnose_2 = as.Date(c("2024-02-01", "2024-07-15"))
  )
  expect_message(
    oncoscopR:::.clean_duplicate_columns(df, verbose = TRUE),
    "De-duplicated"
  )
})

test_that("onc_read_tumorboard returns the contracted empty tibble", {
  out <- onc_read_tumorboard(NULL)
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
  expect_identical(
    names(out),
    c("Patient", "Board_Datum", "Tumorboardbeschluss",
      "Verantwortlich", "Erfasst_am")
  )
  expect_s3_class(out$Board_Datum, "Date")
})

test_that("onc_read_therapy returns empty + source_label when no file", {
  out <- onc_read_therapy(NULL)
  expect_identical(nrow(out), 0L)
  expect_match(attr(out, "source_label"), "Keine OPS-8-544")
})

test_that("onc_read_diagnostics returns empty + source_label when no file", {
  out <- onc_read_diagnostics(NULL)
  expect_identical(nrow(out), 0L)
  expect_match(attr(out, "source_label"), "Keine OPS-1-941")
})
