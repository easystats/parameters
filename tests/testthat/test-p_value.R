context("p_value")

test_that("p_value", {
  model <- circus::lmerMod_1
  testthat::expect_equal(p_value(model)[[1]], 0.195, tol=0.01)
  testthat::expect_equal(p_value(model, method="kenward")[[1]], 0.227, tol=0.01)

  model <- circus::merMod_1
  testthat::expect_equal(p_value(model)[[1]], 0.065, tol=0.01)

  model <- circus::merMod_2
  testthat::expect_equal(p_value(model)[[1]], 0.0695, tol=0.01)

})