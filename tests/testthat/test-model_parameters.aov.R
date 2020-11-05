.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (require("insight") && require("testthat") && require("lme4") && require("parameters")) {

    if (requireNamespace("effectsize")) {
      unloadNamespace("lmerTest")
      data(iris)
      iris$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(iris))
      iris$Cat2 <- rep(c("A", "B"), length.out = nrow(iris))

      test_that("model_parameters.aov", {
        model <- aov(Sepal.Width ~ Species, data = iris)
        mp <- model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE)
        testthat::expect_equal(sum(mp$df), 149)
        testthat::expect_equal(colnames(mp), c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
                                               "Omega2_partial", "Eta2_partial", "Epsilon2_partial"))

        model <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data = iris)
        testthat::expect_equal(sum(model_parameters(model, omega_squared = "raw", eta_squared = "partial", epsilon_squared = TRUE)$df), 149)

        model <- aov(Sepal.Length ~ Species / Cat1 * Cat2, data = iris)
        testthat::expect_equal(sum(model_parameters(model)$df), 149)
      })

      data(mtcars)
      test_that("model_parameters.anova", {
        model <- anova(lm(Sepal.Width ~ Species, data = iris))
        testthat::expect_equal(sum(model_parameters(model)$df), 149)

        model <- anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = iris))
        testthat::expect_equal(sum(model_parameters(model)$df), 149)

        model <- anova(lmer(wt ~ 1 + (1 | gear), data = mtcars))
        testthat::expect_equal(nrow(model_parameters(model)), 0)

        model <- anova(lmer(wt ~ cyl + (1 | gear), data = mtcars))
        testthat::expect_equal(sum(model_parameters(model)$df), 1)

        model <- anova(lmer(wt ~ drat + cyl + (1 | gear), data = mtcars))
        testthat::expect_equal(sum(model_parameters(model)$df), 2)

        model <- anova(lmer(wt ~ drat * cyl + (1 | gear), data = mtcars))
        testthat::expect_equal(sum(model_parameters(model)$df), 3)

        model <- anova(lmer(wt ~ drat/cyl + (1 | gear), data = mtcars))
        testthat::expect_equal(sum(model_parameters(model)$df), 2)
      })

      if (.runThisTest) {
        test_that("model_parameters.anova", {
          model <- insight::download_model("anova_3")
          testthat::expect_equal(sum(model_parameters(model)$df), 149)

          model <- insight::download_model("anova_4")
          testthat::expect_equal(sum(model_parameters(model)$df, na.rm = TRUE), 2)

          model <- insight::download_model("anova_lmerMod_5")
          testthat::expect_equal(sum(model_parameters(model)$df), 1)

          model <- insight::download_model("anova_lmerMod_6")
          testthat::expect_equal(sum(model_parameters(model)$df), 12)
        })
      }

      data(mtcars)
      test_that("model_parameters.anova", {
        model <- aov(wt ~ cyl + Error(gear), data = mtcars)
        testthat::expect_equal(sum(model_parameters(model)$df), 31)

        model <- aov(Sepal.Length ~ Species * Cat1 + Error(Cat2), data = iris)
        testthat::expect_equal(sum(model_parameters(model)$df), 149)

        model <- aov(Sepal.Length ~ Species / Cat1 + Error(Cat2), data = iris)
        testthat::expect_equal(sum(model_parameters(model)$df), 149)
      })
    }
  }
}
