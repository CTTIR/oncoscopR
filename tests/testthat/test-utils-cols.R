test_that(".find_col returns first matching candidate", {
  df <- data.frame(diagnose = "AML", kodierung = "C92.0", name = "Muster")
  expect_identical(
    oncoscopR:::.find_col(df, c("diagnose", "kodierung")),
    "diagnose"
  )
  expect_identical(
    oncoscopR:::.find_col(df, c("kodierung", "diagnose")),
    "kodierung"
  )
})

test_that(".find_col returns NULL when nothing matches (never character(0))", {
  df <- data.frame(a = 1)
  out <- oncoscopR:::.find_col(df, c("b", "c"))
  expect_null(out)
  expect_false(identical(out, character(0)))
})

test_that(".find_col handles empty candidate vector", {
  df <- data.frame(a = 1)
  expect_null(oncoscopR:::.find_col(df, character(0)))
})

test_that(".n_distinct_nonempty drops NA, empty and whitespace-only", {
  x <- c("AML", "AML", "", " ", NA, "CLL")
  expect_identical(oncoscopR:::.n_distinct_nonempty(x), 2L)
})

test_that(".first_nonempty_col coalesces in priority order", {
  df <- data.frame(
    patient    = c(NA, "B", ""),
    patient_2  = c("A1", NA, "C2"),
    stringsAsFactors = FALSE
  )
  out <- oncoscopR:::.first_nonempty_col(df, c("patient", "patient_2"))
  expect_identical(out, c("A1", "B", "C2"))
})

test_that(".first_nonempty_col handles missing columns and empty data", {
  expect_identical(
    oncoscopR:::.first_nonempty_col(data.frame(a = character()), c("a", "b")),
    character(0)
  )
  expect_identical(
    oncoscopR:::.first_nonempty_col(data.frame(a = 1:3), character(0)),
    rep(NA_character_, 3)
  )
})
