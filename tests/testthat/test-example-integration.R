test_that("onc_example_path returns a real file", {
  p <- onc_example_path()
  expect_true(file.exists(p))
  expect_match(p, "onc_example\\.xlsx$")
})

test_that("onc_read_cohort loads the bundled example", {
  df <- onc_read_cohort(onc_example_path(), verbose = FALSE)
  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 0L)
  expect_true("behandlungsjahr" %in% names(df))
})

test_that("onc_read_therapy + prepare counts therapy blocks", {
  raw <- onc_read_therapy(onc_example_path(), verbose = FALSE)
  expect_s3_class(raw, "data.frame")
  blocks <- onc_prepare_therapy_blocks(raw)
  expect_s3_class(blocks, "data.frame")
  expect_true(all(
    c("therapieprotokoll", "diagnose", "patient", "jahr") %in% names(blocks)
  ))
})

test_that("onc_read_diagnostics + prepare counts diagnostic blocks", {
  raw <- onc_read_diagnostics(onc_example_path(), verbose = FALSE)
  expect_s3_class(raw, "data.frame")
  blocks <- onc_prepare_diagnostic_blocks(raw)
  expect_s3_class(blocks, "data.frame")
  expect_true(all(
    c("patient", "diagnose", "jahr") %in% names(blocks)
  ))
})

test_that("onc_parse_oncoprint runs on the bundled example", {
  df <- onc_read_cohort(onc_example_path(), verbose = FALSE)
  skip_if_not(
    "krankheitsspezifische_hematol_resultate" %in% names(df),
    "Example cohort lacks the mutation column."
  )
  out <- onc_parse_oncoprint(df, remove_negative = TRUE)
  expect_s3_class(out, "data.frame")
})
