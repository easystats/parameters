skip_if_offline()
.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (requiet("lme4")) {
    if (requiet("effectsize")) {
      unloadNamespace("afex")
      unloadNamespace("lmerTest")
      data(iris)
      iris$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(iris))
      iris$Cat2 <- rep(c("A", "B"), length.out = nrow(iris))

      # aov ----------------------------------

      test_that("model_parameters.aov", {
        skip_if_not_installed("effectsize", minimum_version = "0.5.0")
        model <- aov(Sepal.Width ~ Species, data = iris)
        mp <- suppressMessages(model_parameters(model, effectsize_type = c("omega", "eta", "epsilon")))
        expect_equal(mp$Parameter, c("Species", "Residuals"))
        expect_equal(mp$Sum_Squares, c(11.34493, 16.962), tolerance = 1e-3)
      })

      test_that("model_parameters.aov", {
        model <- aov(Sepal.Width ~ Species, data = iris)
        mp <- suppressMessages(model_parameters(model, effectsize_type = c("omega", "eta", "epsilon")))
        expect_equal(sum(mp$df), 149)
        expect_equal(colnames(mp), c(
          "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
          "Omega2", "Eta2", "Epsilon2"
        ))

        model <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data = iris)
        expect_equal(sum(model_parameters(model, effectsize_type = c("omega", "eta", "epsilon"), verbose = FALSE)$df), 149)

        model <- aov(Sepal.Length ~ Species / Cat1 * Cat2, data = iris)
        expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 149)
      })

      data(mtcars)
      test_that("model_parameters.anova", {
        model <- anova(lm(Sepal.Width ~ Species, data = iris))
        expect_equal(sum(model_parameters(model)$df), 149)

        model <- anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = iris))
        expect_equal(sum(model_parameters(model)$df), 149)

        model <- anova(lmer(wt ~ 1 + (1 | gear), data = mtcars))
        expect_equal(nrow(model_parameters(model)), 0)

        model <- anova(lmer(wt ~ cyl + (1 | gear), data = mtcars))
        expect_equal(sum(model_parameters(model)$df), 1)

        model <- anova(lmer(wt ~ drat + cyl + (1 | gear), data = mtcars))
        expect_equal(sum(model_parameters(model)$df), 2)

        model <- anova(lmer(wt ~ drat * cyl + (1 | gear), data = mtcars))
        expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 3)

        model <- anova(lmer(wt ~ drat / cyl + (1 | gear), data = mtcars))
        expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 2)
      })

      if (.runThisTest && requiet("httr")) {
        test_that("model_parameters.anova", {
          model <- insight::download_model("anova_3")
          expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 149)

          model <- insight::download_model("anova_4")
          expect_equal(sum(model_parameters(model, verbose = FALSE)$df, na.rm = TRUE), 2)

          model <- insight::download_model("anova_lmerMod_5")
          expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 1)

          model <- insight::download_model("anova_lmerMod_6")
          expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 12)
        })
      }

      data(mtcars)
      test_that("model_parameters.anova", {
        model <- aov(wt ~ cyl + Error(gear), data = mtcars)
        expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 31)

        model <- aov(Sepal.Length ~ Species * Cat1 + Error(Cat2), data = iris)
        expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 149)

        model <- aov(Sepal.Length ~ Species / Cat1 + Error(Cat2), data = iris)
        expect_equal(sum(model_parameters(model, verbose = FALSE)$df), 149)
      })
    }
  }
}
