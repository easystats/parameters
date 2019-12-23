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

    set.seed(123)
    model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)
    testthat::expect_equal(parameters::model_parameters(model)$Median, c(5.8431, -0.8266, 0.092, 0.734, 0.2681, 2.0415), tolerance = 1e-3)

    df <- mtcars
    df$gear <- as.factor(df$gear)
    df$am <- as.factor(df$am)

    model <- BayesFactor::ttestBF(formula = mpg ~ am, data = df)
    testthat::expect_equal(parameters::model_parameters(model)$BF, 86, tolerance = 2)

    set.seed(123)
    model <- BayesFactor::anovaBF(mpg ~ gear * am, data = df)
    testthat::expect_equal(parameters::model_parameters(model)$Median, c(20.6928, -3.2401, 3.2401, 25.2808, 0.7933), tolerance = 1e-3)
  })
}