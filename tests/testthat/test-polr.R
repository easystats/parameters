skip_on_cran()
skip_if_not_installed("MASS")

test_that("robust-se polr", {
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  out <- model_parameters(m)
  expect_identical(attributes(out)$coefficient_name, "Log-Odds")
  m <- MASS::polr(
    Sat ~ Infl + Type + Cont,
    weights = Freq,
    data = housing,
    method = "probit"
  )
  out <- model_parameters(m)
  expect_identical(attributes(out)$coefficient_name, "Z-Score")
})
