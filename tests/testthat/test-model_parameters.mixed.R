context("model_parameters.mixed")

test_that("model_parameters.mixed", {
  params <- model_parameters(circus::download_model("lmerMod_1"), standardize = TRUE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 9))

  params <- model_parameters(circus::download_model("merMod_1"), standardize = TRUE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 9))

  params <- model_parameters(circus::download_model("merMod_2"), standardize = TRUE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 9))

  # TODO: Not sure how to deal with bootstrapped mixed models... As it throws a reasonable amount of singular fits...

})
