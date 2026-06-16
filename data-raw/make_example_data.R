# Generate the bundled synthetic example workbook for zhncommandR.
#
# Source-of-truth script for inst/extdata/zhn_example.xlsx. Re-run after any
# change to the cleaned-column contract the dashboard depends on.
#
# 100% synthetic — no real patient data. Names are "Muster, Fall NNN".
# Buildignored (see .Rbuildignore).

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install 'openxlsx' to regenerate the example data.")
}

set.seed(20260101)

n_patients   <- 100
n_therapy    <- 1000
n_diagnostic <- 130

diag_pool <- c(
  "Multiples Myelom", "Hodgkin-Lymphom", "AML", "CLL",
  "DLBCL", "Follikuläres Lymphom", "MDS", "Mantelzell-Lymphom",
  "ALL", "Marginalzonen-Lymphom"
)
ops_codes <- c(
  "Multiples Myelom"      = "C90.00",
  "Hodgkin-Lymphom"       = "C81.9",
  "AML"                   = "C92.0",
  "CLL"                   = "C91.1",
  "DLBCL"                 = "C83.3",
  "Follikuläres Lymphom"  = "C82.9",
  "MDS"                   = "D46.9",
  "Mantelzell-Lymphom"    = "C83.1",
  "ALL"                   = "C91.0",
  "Marginalzonen-Lymphom" = "C88.4"
)
therapy_protocols <- c(
  "R-CHOP", "VRD", "Rd", "ABVD", "BEACOPP", "FCR",
  "AraC", "7+3", "Azacitidin", "R-Bendamustin", "DA-EPOCH-R"
)

cytogenetics_pool <- c(
  "del(17p)", "del(13q)", "t(11;14)", "t(14;16)", "Trisomie 12",
  "komplexer Karyotyp", "negativ", "normal", "t(8;14)", "del(11q)",
  "Trisomie 8"
)
mutations_pool <- c(
  "TP53 Mutation", "NRAS Mutation", "KRAS Mutation", "DNMT3A Mutation",
  "TET2 Mutation", "FLT3-ITD", "NPM1 Mutation", "IDH1 Mutation",
  "del(13q)", "del(17p)", "kein Nachweis", "Wildtyp"
)

yesno_na <- function(n, p_yes = 0.5, p_na = 0.05) {
  out <- ifelse(stats::runif(n) < p_yes, "ja", "nein")
  out[stats::runif(n) < p_na] <- NA_character_
  out
}

random_dates <- function(n, from = "2022-01-01", to = "2026-05-31") {
  as.Date(from) + sample.int(
    as.integer(as.Date(to) - as.Date(from)), size = n, replace = TRUE
  )
}

random_alterations <- function(pool, n_max = 3) {
  vapply(
    seq_len(n_patients),
    function(.) {
      k <- sample.int(n_max + 1L, 1L) - 1L
      if (k == 0L) return(NA_character_)
      paste(sample(pool, size = k, replace = FALSE), collapse = ", ")
    },
    character(1L)
  )
}

patient_id <- sprintf("Muster, Fall %03d", seq_len(n_patients))
patient_diag <- sample(diag_pool, n_patients, replace = TRUE)

erstvorstellung <- random_dates(n_patients, "2022-01-01", "2025-12-31")
erstdiagnose <- erstvorstellung -
  sample(0:90, n_patients, replace = TRUE)
last_follow_up <- erstvorstellung +
  sample(30:1100, n_patients, replace = TRUE)

pfs <- pmax(1, stats::rexp(n_patients, rate = 1 / 24))
os  <- pfs + pmax(0, stats::rexp(n_patients, rate = 1 / 36))

rezidiv_event <- sample(c(0, 1, NA), n_patients, replace = TRUE,
                        prob = c(0.55, 0.4, 0.05))
death_event   <- sample(c(0, 1, NA), n_patients, replace = TRUE,
                        prob = c(0.75, 0.2, 0.05))

basisdaten <- data.frame(
  name              = patient_id,
  geschlecht        = sample(c("m", "w", "d"), n_patients,
                             replace = TRUE, prob = c(0.5, 0.48, 0.02)),
  geb_datum         = as.Date("1940-01-01") +
                       sample.int(28000L, n_patients, replace = TRUE),
  erstvorstellung   = erstvorstellung,
  erstdiagnose      = erstdiagnose,
  diagnose          = patient_diag,
  kodierung         = unname(ops_codes[patient_diag]),
  primaerfall       = yesno_na(n_patients, p_yes = 0.7),
  patientenfall     = yesno_na(n_patients, p_yes = 0.9),
  tumorkonferenz    = yesno_na(n_patients, p_yes = 0.85),
  fallbesprechung   = yesno_na(n_patients, p_yes = 0.6),
  psychoonkologie   = yesno_na(n_patients, p_yes = 0.55),
  sozialdienst      = yesno_na(n_patients, p_yes = 0.45),
  studie            = yesno_na(n_patients, p_yes = 0.2),
  zahnarzt_mkg      = yesno_na(n_patients, p_yes = 0.3),
  bisphosphonate_denosumab = yesno_na(n_patients, p_yes = 0.4),
  hiv_hepatitis     = yesno_na(n_patients, p_yes = 0.92),
  histologie_inhouse = yesno_na(n_patients, p_yes = 0.7),
  histologie_referenzpathologie = yesno_na(n_patients, p_yes = 0.5),
  komplexe_diagnostik_nach_ops_1_940 = yesno_na(n_patients, p_yes = 0.6),
  therapie_bwk      = yesno_na(n_patients, p_yes = 0.55),
  therapie_inhouse  = yesno_na(n_patients, p_yes = 0.65),
  rezidiv           = ifelse(rezidiv_event == 1, "ja",
                             ifelse(rezidiv_event == 0, "nein", NA)),
  rezidiv_event     = rezidiv_event,
  death_event       = death_event,
  pfs               = round(pfs, 1),
  os                = round(os, 1),
  last_follow_up    = last_follow_up,
  krankheitsspezifische_hematol_resultate = random_alterations(mutations_pool),
  zytogenetik       = random_alterations(cytogenetics_pool),
  stringsAsFactors  = FALSE
)

# --- OPS-8-544 complex chemotherapy ---------------------------------------
therapy_idx <- sample.int(n_patients, n_therapy, replace = TRUE)
komplexe_chemo <- data.frame(
  patient          = patient_id[therapy_idx],
  diagnose         = basisdaten$diagnose[therapy_idx],
  kodierung        = basisdaten$kodierung[therapy_idx],
  therapieprotokoll = sample(therapy_protocols, n_therapy, replace = TRUE),
  ops8544          = sample.int(8L, n_therapy, replace = TRUE),
  zyklusnr         = sample.int(6L, n_therapy, replace = TRUE),
  datum            = random_dates(n_therapy, "2022-01-01", "2026-05-31"),
  stringsAsFactors = FALSE
)

# --- OPS-1-941 complex diagnostics ----------------------------------------
diag_idx <- sample.int(n_patients, n_diagnostic, replace = TRUE)
komplexe_diagnostik <- data.frame(
  patient            = patient_id[diag_idx],
  diagnose           = basisdaten$diagnose[diag_idx],
  kodierung          = basisdaten$kodierung[diag_idx],
  komplexe_diagnostik = sample.int(3L, n_diagnostic, replace = TRUE),
  tumorkonferenz     = random_dates(n_diagnostic, "2022-01-01", "2026-05-31"),
  morphologie        = yesno_na(n_diagnostic, p_yes = 0.85),
  immunphanotypisierung = yesno_na(n_diagnostic, p_yes = 0.7),
  zytogenetik        = yesno_na(n_diagnostic, p_yes = 0.6),
  molekulargenetik   = yesno_na(n_diagnostic, p_yes = 0.5),
  primaerfall        = yesno_na(n_diagnostic, p_yes = 0.7),
  patientenfall      = yesno_na(n_diagnostic, p_yes = 0.9),
  stringsAsFactors = FALSE
)

workbook <- list(
  "Basisdaten"            = basisdaten,
  "Komplexe Chemotherapie" = komplexe_chemo,
  "Komplexe Diagnostik"   = komplexe_diagnostik
)

out_path <- file.path("inst", "extdata", "zhn_example.xlsx")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
openxlsx::write.xlsx(workbook, file = out_path, overwrite = TRUE)
message("Wrote ", out_path, " (", file.info(out_path)$size, " bytes)")

# Also refresh the build-ignored root sample (SimData.xlsx) so a convenient
# upload-test file for the Shiny app exists and stays in sync. Same 100 %
# synthetic content as the bundled example.
sim_path <- "SimData.xlsx"
openxlsx::write.xlsx(workbook, file = sim_path, overwrite = TRUE)
message("Wrote ", sim_path, " (", file.info(sim_path)$size, " bytes)")
