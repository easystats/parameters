if (requiet("testthat") && requiet("parameters") && requiet("marginaleffects") && requiet("rstanarm")) {
  test_that("model_parameters - marginaleffects (frequentist)", {

    # Frequentist
    x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
    model <- marginaleffects::marginaleffects(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
    # x2 <- modelbased::get_emtrends(x, trend = "Petal.Length", at = "Species")

    expect_equal(dim(parameters(model)), c(3, 10))

    # Bayesian
    x <- rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris, refresh = 0, iter = 100, chains = 1)
    model <- marginaleffects::marginaleffects(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
    # x2 <- modelbased::get_emtrends(x, trend = "Petal.Length", at = "Species")

    expect_equal(dim(parameters(model)), c(3, 15))
  })
}
