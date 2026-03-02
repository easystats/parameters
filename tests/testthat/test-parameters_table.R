skip_if_not_installed("effectsize")
skip_if_not_installed("insight")

test_that("parameters_table 1", {
  x <- model_parameters(lm(Sepal.Length ~ Species, data = iris), standardize = "refit")
  tab <- insight::format_table(x)
  expect_equal(colnames(tab), c("Parameter", "Coefficient", "SE", "95% CI", "t(147)", "p"))
})

test_that("parameters_table 2", {
  skip_if_not_installed("lme4")
  x <- model_parameters(lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris), effects = "fixed")
  tab <- insight::format_table(x)
  expect_true(all(names(tab) == c("Parameter", "Coefficient", "SE", "95% CI", "t(146)", "p", "Effects")))
})

test_that("parameters_table 3", {
  x <- effectsize::effectsize(lm(Sepal.Length ~ Species, data = iris))
  tab <- insight::format_table(x)
  expect_equal(colnames(tab), c("Parameter", "Std. Coef.", "95% CI"))
})

test_that("parameters_table 4", {
  x <- model_parameters(lm(Sepal.Length ~ Species, data = iris), standardize = "posthoc")
  tab <- insight::format_table(x)
  expect_equal(colnames(tab), c("Parameter", "Std. Coef.", "SE", "95% CI", "t(147)", "p"))
})

# x <- report::report_table(lm(Sepal.Length ~ Species, data=iris))  # Once on CRAN
# t <- insight::format_table(x)
# t
