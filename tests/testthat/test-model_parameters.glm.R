context("model_parameters.lm")
library(insight)
library(testthat)

test_that("model_parameters.lm", {
  model <- insight::download_model("lm_1")

  params <- model_parameters(model)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))

  params <- model_parameters(model, ci = c(0.8, 0.9))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 10))

  params <- model_parameters(model, dispersion = TRUE, bootstrap = TRUE, n = 500)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 6))

  model <- insight::download_model("lm_2")

  params <- model_parameters(model)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 8))

  model <- insight::download_model("lm_3")

  params <- model_parameters(model)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 8))
})






context("model_parameters.glm")

test_that("model_parameters.glm - binomial", {
  set.seed(333)

  model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

  params <- model_parameters(model)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 8))


  params <- suppressWarnings(model_parameters(model, bootstrap = TRUE, n = 500))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 5))
})
