context("ci")
library(insight)
library(testthat)

test_that("ci", {
  model <- insight::download_model("lm_1")
  testthat::expect_equal(ci(model)[1, 2], 33.4505, tol = 0.01)

  model <- insight::download_model("glm_1")
  testthat::expect_equal(ci(model)[1, 2], 1.934, tol = 0.01)

  model <- insight::download_model("lmerMod_1")
  testthat::expect_equal(ci(model)[1, 2], -0.6250, tol = 0.01)

  val <- ci(model, method = "boot")[1, 2]
  testthat::expect_equal(val, -0.376, tol = 0.25)

  model <- insight::download_model("merMod_1")
  testthat::expect_equal(ci(model)[1, 2], -11.591, tol = 0.01)

  model <- insight::download_model("merMod_2")
  testthat::expect_equal(ci(model)[1, 2], -52.1, tol = 0.01)
})
