# Export + font-registration helpers.

test_that("zhn_register_fonts returns FALSE when systemfonts is unavailable", {
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "systemfonts")) return(FALSE)
      TRUE
    },
    .package = "base"
  )
  expect_false(zhn_register_fonts())
})

test_that("zhn_register_fonts returns FALSE when registration errors", {
  skip_if_not_installed("systemfonts")
  local_mocked_bindings(
    register_font = function(...) stop("boom"),
    .package = "systemfonts"
  )
  res <- zhn_register_fonts()
  expect_false(res)
})

test_that("zhn_register_fonts returns a logical scalar on the real package", {
  res <- zhn_register_fonts()
  expect_true(is.logical(res))
  expect_length(res, 1L)
})

test_that("zhn_save_plot errors when ragg is unavailable for PNG", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar()
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "ragg")) return(FALSE)
      TRUE
    },
    .package = "base"
  )
  tmp <- withr::local_tempfile(fileext = ".png")
  expect_error(
    zhn_save_plot(p, tmp, format = "png"),
    "ragg"
  )
})

test_that("zhn_save_plot rejects an unknown format via match.arg", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) +
    ggplot2::geom_bar()
  tmp <- withr::local_tempfile(fileext = ".svg")
  expect_error(zhn_save_plot(p, tmp, format = "svg"))
})

test_that(".slugify is deterministic and ASCII-only", {
  s1 <- zhncommandR:::.slugify("Kaplan-Meier")
  s2 <- zhncommandR:::.slugify("Kaplan-Meier")
  expect_identical(s1, s2)
  expect_false(grepl("[^a-z0-9_]", s1))
  expect_identical(
    zhncommandR:::.slugify("OPS-8-544: Komplexe Chemotherapie"),
    "ops_8_544_komplexe_chemotherapie"
  )
})
