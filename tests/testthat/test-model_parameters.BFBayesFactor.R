context("model_parameters.BFBayesFactor")

test_that("model_parameters.BFBayesFactor", {
  library(BayesFactor)
  library(logspline)

  model <- BayesFactor::ttestBF(iris$Sepal.Length, iris$Sepal.Width, paired = TRUE)
  testthat::expect_equal(model_parameters(model)$BF, 6.107436e+69, tolerance = 2)
  model <- BayesFactor::ttestBF(formula = mpg ~ am, data = dplyr::mutate(mtcars, am = as.factor(am)))
  testthat::expect_equal(model_parameters(model)$BF, 86, tolerance = 2)
  model <- BayesFactor::correlationBF(iris$Sepal.Length, iris$Petal.Length)
  testthat::expect_equal(model_parameters(model)$BF, 2.136483e+43, tolerance = 2)
  model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)
  testthat::expect_error(model_parameters(model))

  df <- mtcars
  df$gear <- as.factor(df$gear)
  df$am <- as.factor(df$am)
  model <- BayesFactor::anovaBF(mpg ~ gear * am, data = df)
  testthat::expect_error(model_parameters(model))
})
