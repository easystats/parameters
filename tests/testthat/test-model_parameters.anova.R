if (require("insight") && require("testthat") && require("parameters")) {
  data(mtcars)
  m <- glm(am ~ mpg + hp + factor(cyl),
           data = mtcars, family = binomial())

  a <- anova(m, test = "Chisq")
  mp <- model_parameters(a)

  test_that("model_parameters.anova", {
    expect_equal(colnames(mp), c("Parameter", "df", "Deviance", "df_error", "Deviance_error", "p"))
    expect_equal(mp$Deviance_error, c(43.22973, 29.67517, 19.23255, 10.48692), tolerance = 1e-3)
    expect_equal(mp$p, c(NA, 0.00023, 0.00123, 0.01262), tolerance = 1e-3)
  })
}
