if (requiet("testthat") &&
  requiet("parameters") &&
  requiet("survey")) {

  library(survey)
  data(api)
  dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  m <- svytable(~ sch.wide + stype, dclus1)
  mp <- model_parameters(m)
  test_that("model_parameters", {
    expect_equal(colnames(mp), c("F", "df", "df_error", "p", "Method"))
    expect_equal(mp$p, 0.02174746, tolerance = 1e-3)
  })
}
