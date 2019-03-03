context("p_value")

test_that("p_value", {
  model <- circus::download_model("lmerMod_1")
  testthat::expect_equal(p_value(model)[[1]], 0.195, tol=0.01)
  testthat::expect_equal(p_value(model, method="kr")[[1]], 0.227, tol=0.01)

  model <- circus::download_model("merMod_1")
  testthat::expect_equal(p_value(model)[[1]], 0.065, tol=0.01)

  model <- circus::download_model("merMod_2")
  testthat::expect_equal(p_value(model)[[1]], 0.0695, tol=0.01)

})