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

  if (require("car")) {
    a <- car::Anova(m, type = 3, test.statistic = "F")
    mp <- model_parameters(a)

    test_that("model_parameters.anova", {
      expect_equal(colnames(mp), c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p"))
      expect_equal(mp[["F"]], c(53.40138, 60.42944, 13.96887, NA), tolerance = 1e-3)
    })


    m <- lm(cbind(hp, mpg) ~ factor(cyl) * am, data = mtcars)
    a <- car::Anova(m, type = 3, test.statistic = "Pillai")
    mp <- model_parameters(a)

    test_that("model_parameters_Anova.mlm", {
      expect_equal(colnames(mp), c("Parameter", "df", "Statistic", "df_num", "df_error", "F", "p"))
      expect_equal(mp[["F"]], c(158.2578, 6.60593, 3.71327, 3.28975), tolerance = 1e-3)
      expect_equal(mp$Statistic, c(0.9268, 0.67387, 0.22903, 0.4039), tolerance = 1e-3)
    })


    if (require("MASS")) {
      data(housing)
      m <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
      a <- car::Anova(m)
      mp <- model_parameters(a)

      test_that("model_parameters_Anova.mlm", {
        expect_equal(colnames(mp), c("Parameter", "Chi2", "df", "p"))
        expect_equal(mp$Chi2, c(108.2392, 55.91008, 14.30621), tolerance = 1e-3)
      })
    }
  }
}
