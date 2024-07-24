skip_on_cran()

iris$Cat1 <- rep_len(c("X", "X", "Y"), nrow(iris))
iris$Cat2 <- rep_len(c("A", "B"), nrow(iris))

# aov ----------------------------------

test_that("model_parameters.aov", {
  skip_if_not_installed("effectsize", minimum_version = "0.5.0")
  model <- aov(Sepal.Width ~ Species, data = iris)
  mp <- suppressMessages(model_parameters(model, es_type = c("omega", "eta", "epsilon")))
  expect_identical(mp$Parameter, c("Species", "Residuals"))
  expect_equal(mp$Sum_Squares, c(11.34493, 16.962), tolerance = 1e-3)
})

test_that("model_parameters.aov", {
  skip_if_not_installed("effectsize", minimum_version = "0.5.0")
  model <- aov(Sepal.Width ~ Species, data = iris)
  mp <- suppressMessages(model_parameters(model, es_type = c("omega", "eta", "epsilon")))
  expect_identical(sum(mp$df), 149)
  expect_named(mp, c(
    "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p",
    "Omega2", "Eta2", "Epsilon2"
  ))

  model <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data = iris)
  expect_identical(sum(model_parameters(model, es_type = c("omega", "eta", "epsilon"), verbose = FALSE)$df), 149)

  model <- aov(Sepal.Length ~ Species / Cat1 * Cat2, data = iris)
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 149)
})

test_that("model_parameters.anova", {
  skip_if_not_installed("lme4")
  model <- anova(lm(Sepal.Width ~ Species, data = iris))
  expect_identical(sum(model_parameters(model)$df), 149L)

  model <- anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = iris))
  expect_identical(sum(model_parameters(model)$df), 149L)

  model <- anova(lme4::lmer(wt ~ 1 + (1 | gear), data = mtcars))
  expect_identical(nrow(model_parameters(model)), 0L)

  model <- anova(lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars))
  expect_identical(sum(model_parameters(model)$df), 1L)

  model <- anova(lme4::lmer(wt ~ drat + cyl + (1 | gear), data = mtcars))
  expect_identical(sum(model_parameters(model)$df), 2L)

  model <- anova(lme4::lmer(wt ~ drat * cyl + (1 | gear), data = mtcars))
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 3L)

  model <- anova(lme4::lmer(wt ~ drat / cyl + (1 | gear), data = mtcars))
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 2L)
})


test_that("model_parameters.anova", {
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")

  model <- insight::download_model("anova_3")
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 149L)

  model <- insight::download_model("anova_4")
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df, na.rm = TRUE), 2)

  model <- insight::download_model("anova_lmerMod_5")
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 1L)

  model <- insight::download_model("anova_lmerMod_6")
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 12)
})


test_that("model_parameters.anova", {
  model <- aov(wt ~ cyl + Error(gear), data = mtcars)
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 31)

  model <- aov(Sepal.Length ~ Species * Cat1 + Error(Cat2), data = iris)
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 149)

  model <- aov(Sepal.Length ~ Species / Cat1 + Error(Cat2), data = iris)
  expect_identical(sum(model_parameters(model, verbose = FALSE)$df), 149)
})
