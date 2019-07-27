context("model_parameters.lm")
library(insight)
library(testthat)

test_that("model_parameters.lm", {
  model <- insight::download_model("lm_1")

  params <- model_parameters(model, standardize = "refit")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 9))

  params <- model_parameters(model, standardize = "full", ci = c(0.8, 0.9))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 11))

  params <- model_parameters(model, dispersion = TRUE, bootstrap = TRUE, n = 500)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 7))

  model <- insight::download_model("lm_2")

  params <- model_parameters(model, standardize = "2sd")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 9))

  model <- insight::download_model("lm_3")

  params <- model_parameters(model, standardize = "full")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 9))
})






context("model_parameters.glm")

test_that("model_parameters.glm - binomial", {
  set.seed(333)

  model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

  params <- model_parameters(model, standardize = "refit")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 9))


  params <- suppressWarnings(model_parameters(model, standardize = "refit", bootstrap = TRUE, n = 500))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 6))
})
