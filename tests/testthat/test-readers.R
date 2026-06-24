test_that(".check_file_exists aborts cleanly on missing file", {
  expect_error(zhncommandR:::.check_file_exists(NULL), "No file path supplied")
  expect_error(zhncommandR:::.check_file_exists(""), "No file path supplied")
  expect_error(zhncommandR:::.check_file_exists("/no/such/file.xlsx"),
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
    zhncommandR:::.resolve_sheet(tmp, "cohort"),
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
    zhncommandR:::.resolve_sheet(tmp, "cohort"),
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
    zhncommandR:::.resolve_sheet(tmp, "cohort", sheet = "Custom"),
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
    zhncommandR:::.resolve_sheet(tmp, "cohort"),
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
  out <- zhncommandR:::.clean_duplicate_columns(df, verbose = FALSE)
  expect_identical(names(out), c("name", "other"))
})

test_that(".clean_duplicate_columns warns about surviving _2 suffixed cols", {
  df <- data.frame(
    erstdiagnose   = as.Date(c("2024-01-01", "2024-06-15")),
    erstdiagnose_2 = as.Date(c("2024-02-01", "2024-07-15"))
  )
  expect_message(
    zhncommandR:::.clean_duplicate_columns(df, verbose = TRUE),
    "De-duplicated"
  )
})

test_that("zhn_read_tumorboard returns the contracted empty tibble", {
  out <- zhn_read_tumorboard(NULL)
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
  expect_identical(
    names(out),
    c("Patient", "Board_Datum", "Tumorboardbeschluss",
      "Verantwortlich", "Erfasst_am")
  )
  expect_s3_class(out$Board_Datum, "Date")
})

test_that("zhn_read_therapy returns empty + source_label when no file", {
  out <- zhn_read_therapy(NULL)
  expect_identical(nrow(out), 0L)
  expect_match(attr(out, "source_label"), "Keine OPS-8-544")
})

test_that("zhn_read_diagnostics returns empty + source_label when no file", {
  out <- zhn_read_diagnostics(NULL)
  expect_identical(nrow(out), 0L)
  expect_match(attr(out, "source_label"), "Keine OPS-1-941")
})

test_that(".resolve_sheet errors when an explicit sheet is absent", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Basisdaten = data.frame(a = 1)), file = tmp)
  expect_error(
    zhncommandR:::.resolve_sheet(tmp, "cohort", sheet = "Nope"),
    "not found"
  )
})

test_that(".resolve_sheet excludes foreign canonicals in the diagnostics regex", {
  skip_if_not_installed("openxlsx")
  # A workbook where a 'Basisdaten' sheet would match the loose regex but must
  # be excluded for the diagnostics role; a genuine diagnostics sheet wins.
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Basisdaten = data.frame(a = 1),
      "Komplexe Diagnostik 1941" = data.frame(b = 1)
    ),
    file = tmp
  )
  expect_identical(
    zhncommandR:::.resolve_sheet(tmp, "diagnostics"),
    "Komplexe Diagnostik 1941"
  )
})

test_that(".resolve_sheet excludes foreign canonicals in the therapy regex", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      Basisdaten = data.frame(a = 1),
      "OPS 8544 Chemotherapie" = data.frame(b = 1)
    ),
    file = tmp
  )
  expect_identical(
    zhncommandR:::.resolve_sheet(tmp, "therapy"),
    "OPS 8544 Chemotherapie"
  )
})

test_that("zhn_read_therapy returns the empty label when no therapy sheet", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Random = data.frame(a = 1)), file = tmp)
  out <- zhn_read_therapy(tmp, verbose = FALSE)
  expect_identical(nrow(out), 0L)
  expect_match(attr(out, "source_label"), "Keine OPS-8-544")
})

test_that("zhn_read_diagnostics returns the empty label when no diag sheet", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(Random = data.frame(a = 1)), file = tmp)
  out <- zhn_read_diagnostics(tmp, verbose = FALSE)
  expect_identical(nrow(out), 0L)
  expect_match(attr(out, "source_label"), "Keine OPS-1-941")
})

test_that("zhn_read_therapy / _diagnostics read real sheets + carry attrs", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      "Komplexe Chemotherapie" = data.frame(
        patient = c("A", "B"), ops8544 = c(1, 1),
        stringsAsFactors = FALSE
      ),
      "Komplexe Diagnostik" = data.frame(
        patient = c("A", "B"), komplexe_diagnostik = c(1, 1),
        morphologie = c("ja", "ja"), stringsAsFactors = FALSE
      )
    ),
    file = tmp
  )
  th <- zhn_read_therapy(tmp, verbose = FALSE)
  expect_identical(nrow(th), 2L)
  expect_match(attr(th, "source_label"), "Komplexe Chemotherapie")
  expect_identical(attr(th, "sheet_to_use"), "Komplexe Chemotherapie")

  di <- zhn_read_diagnostics(tmp, verbose = FALSE)
  expect_identical(nrow(di), 2L)
  expect_match(attr(di, "source_label"), "Komplexe Diagnostik")
  expect_identical(attr(di, "sheet_to_use"), "Komplexe Diagnostik")
})

test_that("zhn_read_tumorboard parses a real CSV and coerces Board_Datum", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "Patient,Board_Datum,Tumorboardbeschluss,Verantwortlich,Erfasst_am",
    "A,2024-01-15,Weiter,Dr X,2024-01-16",
    "B,2024-02-20,Stop,Dr Y,2024-02-21"
  ), tmp)
  out <- zhn_read_tumorboard(tmp)
  expect_identical(nrow(out), 2L)
  expect_s3_class(out$Board_Datum, "Date")
  expect_identical(out$Patient, c("A", "B"))
})

test_that("zhn_read_tumorboard returns the empty contract for an empty CSV", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    "Patient,Board_Datum,Tumorboardbeschluss,Verantwortlich,Erfasst_am", tmp
  )
  out <- zhn_read_tumorboard(tmp)
  expect_identical(nrow(out), 0L)
  expect_identical(
    names(out),
    c("Patient", "Board_Datum", "Tumorboardbeschluss",
      "Verantwortlich", "Erfasst_am")
  )
})

test_that("zhn_read_tumorboard warns and returns empty on an unreadable file", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines("ok", tmp)
  local_mocked_bindings(
    read.csv = function(...) stop("parse failure"),
    .package = "utils"
  )
  expect_warning(
    out <- zhn_read_tumorboard(tmp),
    "Failed to parse"
  )
  expect_identical(nrow(out), 0L)
})
