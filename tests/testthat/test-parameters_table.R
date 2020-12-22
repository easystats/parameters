if (require("testthat") && require("parameters") && require("insight") && require("effectsize") && require("lme4")) {
  x <- model_parameters(lm(Sepal.Length ~ Species, data = iris), standardize = "refit")
  t <- format_table(x)
  expect_true(all(names(t) == c("Parameter", "Coefficient", "SE", "95% CI", "t(147)", "p")))

  x <- model_parameters(lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris))
  t <- format_table(x)
  expect_true(all(names(t) == c("Parameter", "Coefficient", "SE", "95% CI", "t(146)", "p")))

  x <- effectsize::effectsize(lm(Sepal.Length ~ Species, data = iris))
  t <- format_table(x)
  expect_true(all(names(t) == c("Parameter", "Std. Coef.", "95% CI")))

  x <- model_parameters(lm(Sepal.Length ~ Species, data = iris), standardize = "posthoc")
  t <- format_table(x)
  expect_true(all(names(t) == c("Parameter", "Std. Coef.", "SE", "95% CI", "t(147)", "p")))

  # x <- report::report_table(lm(Sepal.Length ~ Species, data=iris))  # Once on CRAN
  # t <- format_table(x)
  # t
}
