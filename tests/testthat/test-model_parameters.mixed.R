context("model_parameters.mixed")
library(insight)
library(testthat)

test_that("model_parameters.mixed", {
  params <- model_parameters(insight::download_model("lmerMod_1"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 7))

  params <- model_parameters(insight::download_model("lmerMod_1"), ci = c(0.8, 0.9))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 9))

  params <- model_parameters(insight::download_model("merMod_1"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 7))

  params <- model_parameters(insight::download_model("merMod_2"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 7))

  # TODO: Not sure how to deal with bootstrapped mixed models... As it throws an unreasonable amount of singular fits...
})
