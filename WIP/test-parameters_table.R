if (require("testthat") && require("parameters") && require("effectsize") && require("lme4")) {
  x <- parameters::model_parameters(lm(Sepal.Length ~ Species, data = iris), standardize = "refit")
  t <- parameters::parameters_table(x)
  expect_true(all(names(t) == c("Parameter", "Coefficient", "SE", "95% CI", "t(147)", "p")))

  x <- parameters::model_parameters(lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris))
  t <- parameters::parameters_table(x)
  expect_true(all(names(t) == c("Parameter", "Coefficient", "SE", "95% CI", "t(146)", "p")))

  x <- effectsize::effectsize(lm(Sepal.Length ~ Species, data = iris))
  t <- parameters::parameters_table(x)
  expect_true(all(names(t) == c("Parameter", "Std. Coef.", "95% CI")))

  x <- parameters::model_parameters(lm(Sepal.Length ~ Species, data = iris), standardize = "posthoc")
  t <- parameters::parameters_table(x)
  expect_true(all(names(t) == c("Parameter", "Std. Coef.", "SE", "95% CI", "t(147)", "p")))

  # x <- report::report_table(lm(Sepal.Length ~ Species, data=iris))  # Once on CRAN
  # t <- parameters::parameters_table(x)
  # t
}
