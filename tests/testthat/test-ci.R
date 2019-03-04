context("p_value")

test_that("p_value", {
  model <- circus::download_model("lmerMod_1")
  testthat::expect_equal(ci(model)[1, 1], -0.6250, tol = 0.01)

  val <- ci(model, method = "boot")[1, 1]
  testthat::expect_equal(val, -0.376, tol = 0.25)

  model <- circus::download_model("merMod_1")
  testthat::expect_equal(ci(model)[1, 1], -11.591, tol = 0.01)

  model <- circus::download_model("merMod_2")
  testthat::expect_equal(ci(model)[1, 1], -6.730, tol = 0.01)
})
