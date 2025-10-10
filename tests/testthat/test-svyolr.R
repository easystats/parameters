skip_if_not_installed("MASS")
skip_if_not_installed("survey")
skip_on_cran()

test_that("robust-se polr", {
  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  dclus1 <- update(dclus1, mealcat = cut(meals, c(0, 25, 50, 75, 100)))

  m <- survey::svyolr(mealcat ~ avg.ed + mobility + stype, design = dclus1)
  out <- model_parameters(m)
  expect_identical(attributes(out)$coefficient_name, "Log-Odds")
  expect_identicl(
    out$Component,
    c("alpha", "alpha", "alpha", "beta", "beta", "beta", "beta")
  )
  expect_identicl(
    out$Parameter,
    c(
      "(0,25]|(25,50]",
      "(25,50]|(50,75]",
      "(50,75]|(75,100]",
      "avg.ed",
      "mobility",
      "stypeH",
      "stypeM"
    )
  )
  expect_named(
    out,
    c(
      "Parameter",
      "Coefficient",
      "SE",
      "CI",
      "CI_low",
      "CI_high",
      "t",
      "df_error",
      "p",
      "Component"
    )
  )
})
