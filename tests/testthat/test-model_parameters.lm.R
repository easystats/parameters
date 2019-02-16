context("model_parameters.lm")

test_that("model_parameters.lm", {
  library(circus)
  model <- circus::lm_1

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 12)

  params <- model_parameters(model, standardize = TRUE, bootstrap = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map", n = 500))
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 19)

  model <- circus::lm_2

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 4)
  testthat::expect_equal(ncol(params), 12)

  model <- circus::lm_3

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 4)
  testthat::expect_equal(ncol(params), 12)
})
