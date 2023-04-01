test_that("model_parameters", {
  skip_if_not_installed("survey")
  # svychisq is called in model_parameters
  svychisq <<- survey::svychisq

  data(api, package = "survey")
  dclus1 <- survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  m <- survey::svytable(~ sch.wide + stype, dclus1)
  mp <- model_parameters(m)
  expect_equal(colnames(mp), c("F", "df", "df_error", "p", "Method"))
  expect_equal(mp$p, 0.02174746, tolerance = 1e-3)
})
