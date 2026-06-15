test_that("new_cohort_df returns an S3 inheriting from data.frame", {
  df <- data.frame(name = c("A", "B"), diagnose = c("AML", "MM"))
  out <- new_cohort_df(df, sheet_to_use = "Basisdaten")
  expect_s3_class(out, "cohort_df")
  expect_s3_class(out, "data.frame")
  expect_identical(attr(out, "sheet_to_use"), "Basisdaten")
})

test_that("new_cohort_df rejects non-data-frame input", {
  expect_error(new_cohort_df(list()), "must be a data frame")
})

test_that("print.cohort_df returns invisibly", {
  df <- new_cohort_df(data.frame(name = "A", diagnose = "AML"),
                      sheet_to_use = "Basisdaten")
  expect_invisible(print(df))
  # cli writes the h2 header to stderr; we capture both streams here.
  out <- capture.output(print(df), type = "message")
  expect_match(paste(out, collapse = "\n"), "cohort", ignore.case = TRUE)
})

test_that("summary.cohort_df returns one-row data.frame", {
  df <- new_cohort_df(
    data.frame(diagnose = c("AML", "MM", "AML"),
               behandlungsjahr = c(2024L, 2024L, 2025L)),
    sheet_to_use = "Basisdaten"
  )
  s <- summary(df)
  expect_s3_class(s, "data.frame")
  expect_identical(nrow(s), 1L)
  expect_identical(s$diagnoses, 2L)
  expect_identical(s$years_covered, 2L)
})

test_that("new_therapy_blocks carries patient_cols_used attribute", {
  out <- new_therapy_blocks(
    data.frame(patient = "A", therapieprotokoll = "R-CHOP",
               diagnose = "DLBCL"),
    patient_cols_used = "patient, patient_2"
  )
  expect_s3_class(out, "therapy_blocks")
  expect_identical(attr(out, "patient_cols_used"),
                   "patient, patient_2")
})

test_that("summary.therapy_blocks counts unique patients + protocols", {
  out <- new_therapy_blocks(
    data.frame(patient = c("A", "A", "B"),
               therapieprotokoll = c("R-CHOP", "R-CHOP", "VRD"),
               diagnose = c("DLBCL", "DLBCL", "MM"))
  )
  s <- summary(out)
  expect_identical(s$blocks, 3L)
  expect_identical(s$patients, 2L)
  expect_identical(s$protocols, 2L)
})

test_that("new_diagnostic_blocks carries component_cols attribute", {
  out <- new_diagnostic_blocks(
    data.frame(patient = "A", diagnose = "MM"),
    component_cols = "morphologie, immunphanotypisierung"
  )
  expect_s3_class(out, "diagnostic_blocks")
  expect_identical(attr(out, "component_cols"),
                   "morphologie, immunphanotypisierung")
})

test_that("parsers return their S3 class on the example data", {
  skip_if_not_installed("openxlsx")
  raw <- onc_read_therapy(onc_example_path(), verbose = FALSE)
  blocks <- onc_prepare_therapy_blocks(raw)
  expect_s3_class(blocks, "therapy_blocks")

  raw_d <- onc_read_diagnostics(onc_example_path(), verbose = FALSE)
  d_blocks <- onc_prepare_diagnostic_blocks(raw_d)
  expect_s3_class(d_blocks, "diagnostic_blocks")
})

test_that("onc_read_cohort returns a cohort_df", {
  out <- onc_read_cohort(onc_example_path(), verbose = FALSE)
  expect_s3_class(out, "cohort_df")
  expect_true(nzchar(attr(out, "sheet_to_use")))
})

test_that("OPS-1-941 component regex excludes psychoonkologische_diagnostik", {
  skip_if_not_installed("openxlsx")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(
    list(
      "Komplexe Diagnostik" = data.frame(
        patient = c("A", "B"),
        diagnose = c("MM", "AML"),
        komplexe_diagnostik = c(1, 1),
        morphologie = c("ja", "ja"),
        psychoonkologische_diagnostik = c("ja", "ja"),
        stringsAsFactors = FALSE
      )
    ),
    file = tmp
  )
  raw <- onc_read_diagnostics(tmp, verbose = FALSE)
  blocks <- onc_prepare_diagnostic_blocks(raw)
  comp <- attr(blocks, "component_cols")
  expect_true(grepl("morphologie", comp))
  expect_false(grepl("psychoonkologische", comp))
})
