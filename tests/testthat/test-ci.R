context("ci")
library(insight)
library(testthat)

test_that("ci", {
  model <- insight::download_model("lm_1")
  testthat::expect_equal(ci(model)[1, 3], 33.4505, tol = 0.01)
  testthat::expect_equal(ci(model, ci = c(0.7, 0.8))[1, 3], 35.304863, tol = 0.01)

  model <- insight::download_model("glm_1")
  testthat::expect_equal(ci(model)[1, 3], 1.934, tol = 0.01)

  model <- insight::download_model("lmerMod_1")
  testthat::expect_equal(ci(model)[1, 3], -0.6250, tol = 0.01)

  val <- ci(model, method = "boot")[1, 3]
  testthat::expect_equal(val, -0.376, tol = 0.25)

  model <- insight::download_model("merMod_1")
  testthat::expect_equal(ci(model)[1, 3], -11.591, tol = 0.01)

  model <- insight::download_model("merMod_2")
  testthat::expect_equal(ci(model)[1, 3], -52.1, tol = 0.01)
})
