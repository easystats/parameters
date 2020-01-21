if (require("testthat") && require("parameters")) {
  test_that("select_parameters", {
    model <- lm(mpg ~ ., data = mtcars)
    x <- select_parameters(model)
    testthat::expect_equal(n_parameters(model) - n_parameters(x), 7)

    # library(lme4)
    # model <- lmer(Sepal.Width ~ Sepal.Length * Petal.Width * Petal.Length + (1 | Species), data = iris)
    # x <- select_parameters(model)
    # testthat::expect_equal(n_parameters(model) - n_parameters(x), 0) # This is broken

    # library(rstanarm)
    # model <- stan_glm(mpg ~ ., data = mtcars, refresh = 0)
    # x <- select_parameters(model, cross_validation = TRUE)
    # testthat::expect_equal(n_parameters(model) - n_parameters(x), 9)
  })
}