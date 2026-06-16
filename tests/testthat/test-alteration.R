test_that("zhn_normalize_alteration strips trailing Mutation/Mut and collapses ws", {
  expect_identical(
    zhn_normalize_alteration(c("TP53 Mutation", "  NRAS  Mut ", "del(17p)")),
    c("TP53", "NRAS", "del(17p)")
  )
})

test_that("zhn_alteration_type classifies the documented categories", {
  alts <- c("TP53 Mutation", "del(17p)", "Trisomie 12", "t(11;14)",
            "komplexer Karyotyp", "negativ", "kein Nachweis", "wildtyp", NA, "")
  expected <- c(
    "Mutation/Variante",
    "Strukturell/Zytogenetik: Deletion/Loss",
    "Strukturell/Zytogenetik: Zugewinn/Amplifikation",
    "Strukturell/Zytogenetik: Translokation/Rearrangement/Bruch",
    "Strukturell/Zytogenetik: Komplexer Karyotyp",
    "negativ/kein Nachweis", "negativ/kein Nachweis", "negativ/kein Nachweis",
    "Nicht verwertbar/NA", "Nicht verwertbar/NA"
  )
  expect_identical(zhn_alteration_type(alts), expected)
})

test_that("zhn_is_mutation is TRUE only for the Mutation/Variante class", {
  classes <- c("Mutation/Variante", "negativ/kein Nachweis",
               "Strukturell/Zytogenetik: Deletion/Loss")
  expect_identical(zhn_is_mutation(classes), c(TRUE, FALSE, FALSE))
})
