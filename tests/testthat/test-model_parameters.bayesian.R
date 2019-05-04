context("model_parameters.bayesian")
library(insight)
library(rstanarm)
library(brms)
library(testthat)


# RSTANARM --------------------------------------------------------------------

test_that("model_parameters.stanreg", {
  set.seed(333)

  # GLM
  params <- model_parameters(insight::download_model("stanreg_lm_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 24))

  params <- model_parameters(insight::download_model("stanreg_lm_2"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 24))

  params <- model_parameters(insight::download_model("stanreg_lm_3"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 24))

  params <- model_parameters(insight::download_model("stanreg_glm_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 24))

  params <- model_parameters(insight::download_model("stanreg_glm_2"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 24))

  # Mixed
  testthat::expect_warning(params <- model_parameters(insight::download_model("stanreg_lmerMod_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map")))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 24))

  params <- model_parameters(insight::download_model("stanreg_merMod_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 24))

  params <- model_parameters(insight::download_model("stanreg_merMod_2"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"), rope_full = FALSE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 24))

  # GAM
  params <- model_parameters(insight::download_model("stanreg_gam_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"), rope_full = FALSE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 24))
})


# BRMS --------------------------------------------------------------------


test_that("model_parameters.brmsfit", {
  skip_on_travis()
  skip_on_cran()

  # LM
  testthat::expect_warning(params <- model_parameters(insight::download_model("brms_mixed_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map")))
  testthat::expect_equal(nrow(params), 2)
  testthat::expect_equal(ncol(params), 19)
})
