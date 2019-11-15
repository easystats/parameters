context("model_parameters.mixed")
library(insight)
library(testthat)

test_that("model_parameters.mixed", {
  params <- model_parameters(insight::download_model("lmerMod_1"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))
  testthat::expect_equal(params$CI_high, c(1.6373105660317, 0.554067677205595), tolerance = 1e-3)

  params <- model_parameters(insight::download_model("lmerMod_1"), ci = c(0.8, 0.9))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 10))
  testthat::expect_equal(params$CI_high_80, c(1.29595665381331, 0.502185700948862), tolerance = 1e-3)
  testthat::expect_equal(params$CI_high_90, c(1.47875781798108, 0.529969433080186), tolerance = 1e-3)

  params <- model_parameters(insight::download_model("merMod_1"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))

  params <- model_parameters(insight::download_model("merMod_2"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 8))

  # TODO: Not sure how to deal with bootstrapped mixed models... As it throws an unreasonable amount of singular fits...
})
