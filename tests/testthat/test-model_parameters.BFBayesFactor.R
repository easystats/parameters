context("model_parameters.BFBayesFactor")

test_that("model_parameters.BFBayesFactor", {
  #testthat::skip_on_travis()

  library(BayesFactor)
  library(logspline)

  model <- BayesFactor::ttestBF(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)
  testthat::expect_equal(parameters::model_parameters(model)$BF, 492.770, tolerance = 2)

  model <- BayesFactor::correlationBF(iris$Sepal.Width, iris$Petal.Length)
  testthat::expect_equal(parameters::model_parameters(model)$BF, 348853.6, tolerance = 10)

  model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)
  testthat::expect_error(parameters::model_parameters(model))



  df <- mtcars
  df$gear <- as.factor(df$gear)
  df$am <- as.factor(df$am)

  model <- BayesFactor::ttestBF(formula = mpg ~ am, data = df)
  testthat::expect_equal(parameters::model_parameters(model)$BF, 86, tolerance = 2)

  model <- BayesFactor::anovaBF(mpg ~ gear * am, data = df)
  testthat::expect_error(parameters::model_parameters(model))
})
