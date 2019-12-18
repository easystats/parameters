if (require("testthat") &&
    require("parameters") &&
    require("BayesFactor") &&
    require("logspline")) {
  test_that("model_parameters.BFBayesFactor", {
    # testthat::skip_on_travis()
    model <- BayesFactor::ttestBF(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)
    testthat::expect_equal(parameters::model_parameters(model)$BF, 492.770, tolerance = 2)

    model <- BayesFactor::correlationBF(iris$Sepal.Width, iris$Petal.Length)
    testthat::expect_equal(parameters::model_parameters(model)$BF, 348853.6, tolerance = 10)

    model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)
    testthat::expect_equal(parameters::model_parameters(model)$Median, c(5.8438, -0.8264, 0.0909, 0.7361, 0.2675, 2.0675), tolerance = 1e-3)

    df <- mtcars
    df$gear <- as.factor(df$gear)
    df$am <- as.factor(df$am)

    model <- BayesFactor::ttestBF(formula = mpg ~ am, data = df)
    testthat::expect_equal(parameters::model_parameters(model)$BF, 86, tolerance = 2)

    model <- BayesFactor::anovaBF(mpg ~ gear * am, data = df)
    testthat::expect_equal(parameters::model_parameters(model)$Median, c(20.6879, -3.2436, 3.2436, 25.5268, 0.7841), tolerance = 1e-3)
  })
}