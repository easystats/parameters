context("efa-cfa")

test_that("efa-cfa", {
  library(psych)
  library(lavaan)

  efa <- psych::fa(attitude, nfactors = 3)
  params <- parameters::model_parameters(efa)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(7, 6))

  model1 <- efa_to_cfa(efa)
  model2 <- efa_to_cfa(efa, threshold = 0.3)

  testthat::expect_equal(nchar(model1), 109)

  m1 <- lavaan::cfa(model1, data = attitude)
  params <- parameters::model_parameters(m1)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(10, 9))

  testthat::expect_warning(parameters::model_parameters(m1, ci = c(0.8, 0.9)))

  params <- parameters::model_parameters(m1, standardize = TRUE, type = "all")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(20, 9))

  x <- anova(m1, lavaan::cfa(model2, data = attitude))
  params <- parameters::model_parameters(x)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 6))
})


test_that("BayesFM", {
  library(BayesFM)

  efa <- BayesFM::befa(mtcars, iter = 1000)
  params <- parameters::model_parameters(efa, sort = TRUE)
  testthat::expect_equal(nrow(params), 11)
})