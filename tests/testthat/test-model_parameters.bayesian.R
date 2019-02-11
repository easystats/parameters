context("model_parameters.bayesian")



# RSTANARM --------------------------------------------------------------------

test_that("model_parameters.stanreg", {
  library(rstanarm)

  # LM
  model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
  params <- model_parameters(model, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 19)

  # LMER
  model <- rstanarm::stan_lmer(mpg ~ wt + (1|gear), data = mtcars)
  params <- model_parameters(model, standardize = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"))
  testthat::expect_equal(nrow(params), 2)
  testthat::expect_equal(ncol(params), 19)
})




# BRMS --------------------------------------------------------------------


test_that("model_parameters.brmsfit", {
  testthat::skip_on_travis()
  library(brms)

  # LM
  model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
  params <- model_parameters(model, standardize=TRUE, estimate=c("median", "mean", "MAP"), test=c("pd", "rope", "p_map"))
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 19)
})
