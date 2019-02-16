context("model_parameters.bayesian")



# RSTANARM --------------------------------------------------------------------

test_that("model_parameters.stanreg", {
  library(circus)
  library(rstanarm)
  # data("stanreg_lm_1", package="circus")

  # GLM
  params <- model_parameters(circus::stanreg_lm_1, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 21))

  params <- model_parameters(circus::stanreg_lm_2, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 21))

  params <- model_parameters(circus::stanreg_lm_3, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 21))

  params <- model_parameters(circus::stanreg_glm_1, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 21))

  params <- model_parameters(circus::stanreg_glm_2, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 21))

  # Mixed
  params <- model_parameters(circus::stanreg_lmerMod_1, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 21))

  params <- model_parameters(circus::stanreg_merMod_1, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 21))

  params <- model_parameters(circus::stanreg_lmerMod_1, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"), rope_full=FALSE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 21))
})




# BRMS --------------------------------------------------------------------


test_that("model_parameters.brmsfit", {
  testthat::skip_on_travis()
  library(brms)

  # LM
  model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
  params <- model_parameters(model, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 19)
})
