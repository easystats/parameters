context("model_parameters.mixed")
library(insight)
library(testthat)

test_that("model_parameters.mixed", {
  params <- model_parameters(insight::download_model("lmerMod_1"), standardize = "refit")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))

  params <- model_parameters(insight::download_model("merMod_1"), standardize = "refit")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))

  params <- model_parameters(insight::download_model("merMod_2"), standardize = "refit")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 8))

  # TODO: Not sure how to deal with bootstrapped mixed models... As it throws a reasonable amount of singular fits...
})
