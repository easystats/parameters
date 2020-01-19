if (require("insight") && require("testthat") && require("parameters")) {
  data(iris)
  iris$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(iris))
  iris$Cat2 <- rep(c("A", "B"), length.out = nrow(iris))

  test_that("model_parameters.aov", {
    model <- aov(Sepal.Width ~ Species, data = iris)
    testthat::expect_equal(sum(model_parameters(model, omega_squared = "partial", eta_squared = "partial", epsilon_squared = TRUE)$df), 149)

    model <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data = iris)
    testthat::expect_equal(sum(model_parameters(model, omega_squared = "raw", eta_squared = "partial", epsilon_squared = TRUE)$df), 149)

    model <- aov(Sepal.Length ~ Species / Cat1 * Cat2, data = iris)
    testthat::expect_equal(sum(model_parameters(model)$df), 149)
  })


  test_that("model_parameters.anova", {
    model <- insight::download_model("anova_1")
    testthat::expect_equal(sum(model_parameters(model)$df), 149)

    model <- insight::download_model("anova_2")
    testthat::expect_equal(sum(model_parameters(model)$df), 149)

    model <- insight::download_model("anova_3")
    testthat::expect_equal(sum(model_parameters(model)$df), 149)

    model <- insight::download_model("anova_4")
    testthat::expect_equal(sum(model_parameters(model)$df, na.rm = TRUE), 2)

    model <- insight::download_model("anova_lmerMod_0")
    testthat::expect_equal(nrow(model_parameters(model)), 0)

    model <- insight::download_model("anova_lmerMod_1")
    testthat::expect_equal(sum(model_parameters(model)$df), 1)

    model <- insight::download_model("anova_lmerMod_2")
    testthat::expect_equal(sum(model_parameters(model)$df), 2)

    model <- insight::download_model("anova_lmerMod_3")
    testthat::expect_equal(sum(model_parameters(model)$df), 3)

    model <- insight::download_model("anova_lmerMod_4")
    testthat::expect_equal(sum(model_parameters(model)$df), 2)

    model <- insight::download_model("anova_lmerMod_5")
    testthat::expect_equal(sum(model_parameters(model)$df), 1)

    model <- insight::download_model("anova_lmerMod_6")
    testthat::expect_equal(sum(model_parameters(model)$df), 12)
  })




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
