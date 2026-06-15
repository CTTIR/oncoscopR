test_that(".as_yesno handles German + English variants and missingness", {
  x <- c("ja", "Ja", "yes", "1", "x",
         "nein", "no", "0",
         NA, "", "n/a", "unbekannt", "10 months")
  expected <- c(rep(TRUE, 5), rep(FALSE, 3), rep(NA, 5))
  expect_identical(oncoscopR:::.as_yesno(x), expected)
})

test_that(".as_yesno passes through logical input", {
  x <- c(TRUE, FALSE, NA)
  expect_identical(oncoscopR:::.as_yesno(x), x)
})

# --- Regression: the `^1` bug in legacy v5 ---------------------------------
#
# v5 used grepl("^1", x_low) — any string starting with "1" was an event.
# We require strict, anchored token matching.
test_that(".as_event01 does NOT classify '10 months' as an event", {
  expect_identical(oncoscopR:::.as_event01("10 months"), NA_real_)
  expect_identical(oncoscopR:::.as_event01("100 Tage"), NA_real_)
})

test_that(".as_event01 still maps explicit 1/0 and German tokens", {
  x <- c("1", "0", "ja", "nein", "tod", "lebt", NA, "", "?")
  expect_identical(
    oncoscopR:::.as_event01(x),
    c(1, 0, 1, 0, 1, 0, NA_real_, NA_real_, NA_real_)
  )
})

test_that(".as_event01 maps Dates: present = 1, missing = 0", {
  d <- as.Date(c("2024-01-15", NA, "2025-06-01"))
  expect_identical(oncoscopR:::.as_event01(d), c(1, 0, 1))
})

test_that(".as_event01 date_event mode treats non-empty strings as events", {
  x <- c("2024-01-15", "", NA, "1")
  expect_identical(
    oncoscopR:::.as_event01(x, mode = "date_event"),
    c(1, 0, 0, 1)
  )
})

test_that(".safe_date passes Date through and parses character", {
  expect_identical(
    oncoscopR:::.safe_date(as.Date("2024-01-15")),
    as.Date("2024-01-15")
  )
  expect_identical(
    oncoscopR:::.safe_date("2024-01-15"),
    as.Date("2024-01-15")
  )
})

test_that(".add_year derives behandlungsjahr from first matching date column", {
  df <- data.frame(
    erstvorstellung = as.Date(c("2023-01-01", "2024-06-15")),
    erstdiagnose    = as.Date(c("2020-01-01", "2021-01-01"))
  )
  out <- oncoscopR:::.add_year(df)
  expect_identical(out$behandlungsjahr, c(2023L, 2024L))
})

test_that(".add_year falls back to NA when no date column present", {
  df <- data.frame(name = c("A", "B"))
  out <- oncoscopR:::.add_year(df)
  expect_identical(out$behandlungsjahr, rep(NA_integer_, 2))
})
