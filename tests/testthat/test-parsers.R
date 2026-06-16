test_that("zhn_prepare_therapy_blocks returns the contracted empty tibble", {
  out <- zhn_prepare_therapy_blocks(data.frame())
  expect_s3_class(out, "data.frame")
  expect_identical(nrow(out), 0L)
  expect_true(all(
    c("therapieprotokoll", "diagnose", "patient", "datum", "zyklus",
      "jahr", "monat_sort") %in% names(out)
  ))
  expect_identical(attr(out, "patient_cols_used"), "")
})

test_that("zhn_prepare_therapy_blocks counts only positive OPS-8-544 blocks", {
  df <- data.frame(
    ops8544 = c(1, 0, 2, NA),
    patient = c("A", "B", "C", "D"),
    therapieprotokoll = c("R-CHOP", "R-CHOP", "VRD", "ABVD"),
    diagnose = c("DLBCL", "DLBCL", "MM", "HL"),
    datum = c("2024-01-15", "2024-02-15", "2024-03-15", "2024-04-15"),
    zyklusnr = c(1, 1, 2, 1),
    stringsAsFactors = FALSE
  )
  out <- zhn_prepare_therapy_blocks(df)
  expect_identical(nrow(out), 2L)
  expect_setequal(out$patient, c("A", "C"))
})

test_that("zhn_prepare_diagnostic_blocks returns contracted empty tibble", {
  out <- zhn_prepare_diagnostic_blocks(data.frame())
  expect_identical(nrow(out), 0L)
  expect_true(all(
    c("patient", "diagnose", "datum", "primaerfall", "patientenfall",
      "jahr", "monat_sort") %in% names(out)
  ))
  expect_identical(attr(out, "component_cols"), "")
})

test_that("zhn_parse_oncoprint returns contracted empty tibble on empty df", {
  out <- zhn_parse_oncoprint(data.frame())
  expect_identical(nrow(out), 0L)
  expect_true(all(
    c("patient_label", "diagnose_label", "alteration", "alteration_class",
      "oncoprint_mutation", "alteration_raw") %in% names(out)
  ))
})

test_that("zhn_parse_oncoprint aborts when required source column missing", {
  df <- data.frame(name = "A", diagnose = "AML")
  expect_error(
    zhn_parse_oncoprint(df),
    "krankheitsspezifische_hematol_resultate"
  )
})

test_that("zhn_parse_oncoprint splits, classifies and removes negatives", {
  df <- data.frame(
    name = c("A", "B"),
    diagnose = c("AML", "MM"),
    krankheitsspezifische_hematol_resultate = c(
      "TP53 Mutation, NRAS Mut",
      "negativ"
    ),
    stringsAsFactors = FALSE
  )
  out <- zhn_parse_oncoprint(df, remove_negative = TRUE)
  expect_identical(nrow(out), 2L)
  expect_true(all(out$oncoprint_mutation))
  expect_setequal(out$alteration, c("TP53", "NRAS"))
})

test_that("therapy parser preserves Date input and derives year+month", {
  df <- data.frame(
    ops8544 = c(1, 1),
    patient = c("A", "B"),
    therapieprotokoll = c("R-CHOP", "VRD"),
    diagnose = c("DLBCL", "MM"),
    datum = as.Date(c("2024-01-15", "2025-06-01")),
    zyklusnr = c(1L, 1L),
    stringsAsFactors = FALSE
  )
  out <- zhn_prepare_therapy_blocks(df)
  expect_identical(out$jahr, c(2024L, 2025L))
  expect_identical(out$monat_sort, c("2024-01", "2025-06"))
})

test_that("zhn_parse_cytogenetics aborts when zytogenetik column missing", {
  df <- data.frame(name = "A", diagnose = "AML")
  expect_error(zhn_parse_cytogenetics(df), "zytogenetik")
})

test_that("zhn_parse_cytogenetics classifies del/trans/complex", {
  df <- data.frame(
    name = c("A", "B", "C"),
    diagnose = c("MM", "AML", "MDS"),
    zytogenetik = c("del(17p)", "t(11;14)", "komplexer Karyotyp"),
    stringsAsFactors = FALSE
  )
  out <- zhn_parse_cytogenetics(df)
  expect_identical(nrow(out), 3L)
  expect_setequal(
    out$alteration_class,
    c("Strukturell/Zytogenetik: Deletion/Loss",
      "Strukturell/Zytogenetik: Translokation/Rearrangement/Bruch",
      "Strukturell/Zytogenetik: Komplexer Karyotyp")
  )
})
