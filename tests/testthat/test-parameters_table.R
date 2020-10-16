if (require("testthat") && require("parameters") && require("effectsize")) {
  x <- parameters::model_parameters(lm(Sepal.Length ~ Species, data=iris), standardize = "refit")
  t <- parameters::parameters_table(x)
  testthat::expect_true(all(names(t) == c("Parameter","Coefficient","SE","95% CI","t","df","p")))

  x <- effectsize::effectsize(lm(Sepal.Length ~ Species, data=iris))
  t <- parameters::parameters_table(x)
  testthat::expect_true(all(names(t) == c("Parameter","Std. Coefficient","95% CI")))

  # x <- report::report_table(lm(Sepal.Length ~ Species, data=iris))  # Once on CRAN
  # t <- parameters::parameters_table(x)
  # t
}
