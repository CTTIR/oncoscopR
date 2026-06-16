# Helper: build a 5-row workbook with the canonical schema, in a tempfile
# scoped to the testthat block. Used by the integration tests so they don't
# depend on the size or exact content of the bundled example workbook.
.make_tiny_workbook <- function(envir = parent.frame()) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    testthat::skip("openxlsx required")
  }
  tmp <- withr::local_tempfile(fileext = ".xlsx", .local_envir = envir)
  openxlsx::write.xlsx(
    list(
      "Basisdaten" = data.frame(
        name = c("Muster, A", "Muster, B", "Muster, C",
                 "Muster, D", "Muster, E"),
        diagnose = c("AML", "MM", "DLBCL", "CLL", "HL"),
        kodierung = c("C92.0", "C90.00", "C83.3", "C91.1", "C81.9"),
        erstvorstellung = as.Date(c("2024-01-01", "2024-04-15", "2025-01-10",
                                    "2025-03-22", "2025-08-08")),
        primaerfall = c("ja", "ja", "nein", "ja", "ja"),
        patientenfall = c("ja", "ja", "ja", "ja", "ja"),
        psychoonkologie = c("ja", "nein", "ja", "ja", "nein"),
        tumorkonferenz = c("ja", "ja", "ja", "nein", "ja"),
        pfs = c(12, 24, 18, 36, 6),
        os = c(15, 30, 24, 48, 9),
        rezidiv_event = c(1, 0, 1, 0, 1),
        death_event = c(1, 0, 0, 0, 1),
        krankheitsspezifische_hematol_resultate = c(
          "TP53 Mutation", "NRAS, DNMT3A", "del(17p)", NA, "kein Nachweis"
        ),
        zytogenetik = c("komplexer Karyotyp", "t(11;14)", "del(17p)",
                        "Trisomie 12", "negativ"),
        stringsAsFactors = FALSE
      ),
      "Komplexe Chemotherapie" = data.frame(
        patient = c("Muster, A", "Muster, A", "Muster, B"),
        diagnose = c("AML", "AML", "MM"),
        therapieprotokoll = c("7+3", "AraC", "VRD"),
        ops8544 = c(1, 1, 1),
        zyklusnr = c(1, 2, 1),
        datum = as.Date(c("2024-01-10", "2024-02-15", "2024-04-20")),
        stringsAsFactors = FALSE
      ),
      "Komplexe Diagnostik" = data.frame(
        patient = c("Muster, A", "Muster, B", "Muster, C"),
        diagnose = c("AML", "MM", "DLBCL"),
        komplexe_diagnostik = c(1, 1, 1),
        tumorkonferenz = as.Date(c("2024-01-05", "2024-04-10", "2025-01-05")),
        morphologie = c("ja", "ja", "ja"),
        immunphanotypisierung = c("ja", "ja", NA),
        stringsAsFactors = FALSE
      )
    ),
    file = tmp
  )
  tmp
}

test_that("zhn_example_path returns a real file", {
  p <- zhn_example_path()
  expect_true(file.exists(p))
  expect_match(p, "zhn_example\\.xlsx$")
})

test_that("integration: tiny fixture roundtrips through every reader", {
  tmp <- .make_tiny_workbook()
  cohort <- zhn_read_cohort(tmp, verbose = FALSE)
  expect_s3_class(cohort, "cohort_df")
  expect_identical(nrow(cohort), 5L)
  expect_true("behandlungsjahr" %in% names(cohort))

  therapy <- zhn_read_therapy(tmp, verbose = FALSE)
  blocks <- zhn_prepare_therapy_blocks(therapy)
  expect_s3_class(blocks, "therapy_blocks")
  expect_identical(nrow(blocks), 3L)

  diag <- zhn_read_diagnostics(tmp, verbose = FALSE)
  d_blocks <- zhn_prepare_diagnostic_blocks(diag)
  expect_s3_class(d_blocks, "diagnostic_blocks")
  expect_identical(nrow(d_blocks), 3L)

  onco <- zhn_parse_oncoprint(cohort)
  expect_gt(nrow(onco), 0L)
  expect_true(all(c("alteration", "alteration_class") %in% names(onco)))

  cyto <- zhn_parse_cytogenetics(cohort)
  expect_gt(nrow(cyto), 0L)
  expect_true("Strukturell/Zytogenetik: Komplexer Karyotyp" %in%
                cyto$alteration_class)
  expect_true(
    "Strukturell/Zytogenetik: Translokation/Rearrangement/Bruch" %in%
      cyto$alteration_class
  )
})

# Bundled-example smoke test stays but is now redundant with the fixture above;
# kept as a regression on the actual shipped file.
test_that("bundled example workbook still loads", {
  cohort <- zhn_read_cohort(zhn_example_path(), verbose = FALSE)
  expect_gt(nrow(cohort), 0L)
})
