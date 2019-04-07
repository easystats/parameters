context("model_parameters.bayesian")



# RSTANARM --------------------------------------------------------------------

test_that("model_parameters.stanreg", {
  library(circus)
  library(rstanarm)

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
  params <- model_parameters(insight::download_model("stanreg_lmerMod_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
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
  testthat::skip_on_travis()
  library(brms)

  # LM
  params <- model_parameters(insight::download_model("brms_mixed_1"), standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(nrow(params), 2)
  testthat::expect_equal(ncol(params), 19)
})
