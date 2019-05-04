context("model_parameters.lm")
library(insight)
library(testthat)

test_that("model_parameters.lm", {
  model <- insight::download_model("lm_1")

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 2)
  testthat::expect_equal(ncol(params), 9)

  params <- model_parameters(model, standardize = TRUE, bootstrap = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map", n = 500))
  testthat::expect_equal(nrow(params), 2)
  testthat::expect_equal(ncol(params), 13)

  model <- insight::download_model("lm_2")

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 9)

  model <- insight::download_model("lm_3")

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 4)
  testthat::expect_equal(ncol(params), 9)
})
