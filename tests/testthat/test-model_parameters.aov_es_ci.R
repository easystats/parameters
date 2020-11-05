if (require("insight") && require("effectsize") && require("testthat") && require("lme4") && require("parameters")) {

  unloadNamespace("lmerTest")
  data(iris)
  iris$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(iris))
  iris$Cat2 <- rep(c("A", "B"), length.out = nrow(iris))

  # aov ----------------------------------

  test_that("model_parameters.aov", {
    model <- aov(Sepal.Width ~ Species, data = iris)
    mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
    es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
    expect_equivalent(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3)
    expect_equivalent(mp$Omega2_CI_low, c(0.29018, NA), tolerance = 1e-3)
    expect_equivalent(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3)

    expect_equal(colnames(mp), c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
                                 "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
                                 "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
                                 "Epsilon2_CI_high"))

    model <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data = iris)
    mp <- model_parameters(model, eta_squared = "raw", ci = .9)
    es <- effectsize::eta_squared(model, partial = FALSE, ci = .9)
    expect_equivalent(na.omit(mp$Eta2_CI_low), es$CI_low, tolerance = 1e-3)
    expect_equivalent(mp$Eta2_CI_low, c(0.53866, 0, 0, 0, 0, 0, 0, NA), tolerance = 1e-3)
    expect_equivalent(na.omit(mp$Eta2_CI_high), es$CI_high, tolerance = 1e-3)

    expect_equal(colnames(mp), c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
                                 "Eta2", "Eta2_CI_low", "Eta2_CI_high"))
  })


  # anova ---------------------

  data(mtcars)
  test_that("model_parameters.anova", {
    model <- anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = iris))
    mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
    es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
    expect_equivalent(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3)
    expect_equivalent(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3)

    expect_equal(colnames(mp), c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
                                 "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
                                 "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
                                 "Epsilon2_CI_high"))
  })

  data(mtcars)
  test_that("model_parameters.anova", {
    model <- aov(wt ~ cyl + Error(gear), data = mtcars)
    mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
    es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
    expect_equivalent(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3)
    expect_equivalent(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3)

    expect_equal(colnames(mp), c("Group", "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
                                 "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
                                 "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
                                 "Epsilon2_CI_high"))
  })


  # car anova ---------------------------------

  if (require("car")) {
    set.seed(123)
    data(Moore)
    mod <-
      car::Anova(stats::lm(
        formula = conformity ~ fcategory * partner.status,
        data = Moore,
        contrasts = list(fcategory = contr.sum, partner.status = contr.sum)
      ))
    test_that("model_parameters.car-anova", {
      mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
      es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
      expect_equivalent(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3)
      expect_equivalent(mp$Omega2_CI_low, c(0.52133, 0, 0, 0, 0, 0, 0, NA), tolerance = 1e-3)
      expect_equivalent(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3)

      expect_equal(colnames(mp), c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
                                   "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
                                   "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
                                   "Epsilon2_CI_high"))
    })
  }


  # maov ----------------------------------

  set.seed(123)
  fit <- lm(cbind(mpg, disp, hp) ~ factor(cyl), data = mtcars)
  model <- aov(fit)

  test_that("model_parameters.maov", {
    mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE, ci = .9)
    es <- effectsize::omega_squared(model, partial = TRUE, ci = .9)
    expect_equivalent(na.omit(mp$Omega2_CI_low), es$CI_low, tolerance = 1e-3)
    expect_equivalent(mp$Omega2_CI_low, c(0.71218, NA, 0.50841, NA, 0.53774, NA), tolerance = 1e-3)
    expect_equivalent(na.omit(mp$Omega2_CI_high), es$CI_high, tolerance = 1e-3)

    expect_equal(colnames(mp), c("Response", "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
                                 "Omega2_partial", "Omega2_CI_low", "Omega2_CI_high", "Eta2_partial",
                                 "Eta2_CI_low", "Eta2_CI_high", "Epsilon2_partial", "Epsilon2_CI_low",
                                 "Epsilon2_CI_high"))
  })
}
