context("model_parameters.bayesian")


# RSTANARM --------------------------------------------------------------------

test_that("model_parameters.stanreg", {
  set.seed(333)
  library(rstanarm)
  library(logspline)


  # P value
  expect_equal(ncol(p_value(insight::download_model("stanreg_lm_1"))), 2)

  # GLM
  params <- model_parameters(insight::download_model("stanreg_lm_1"), centrality = "all", test = "all", dispersion = TRUE)
  expect_equal(c(nrow(params), ncol(params)), c(2, 18))
  expect_equal(params$CI_high, c(40.2985874345282, -4.46283763262213), tolerance = 1e-3)

  params <- model_parameters(insight::download_model("stanreg_lm_2"), centrality = "all", test = "all", dispersion = TRUE)
  expect_equal(c(nrow(params), ncol(params)), c(3, 18))

  params <- model_parameters(insight::download_model("stanreg_lm_3"), centrality = "all", test = "all", dispersion = TRUE)
  expect_equal(c(nrow(params), ncol(params)), c(4, 18))

  params <- model_parameters(insight::download_model("stanreg_glm_1"), centrality = "all", test = "all", dispersion = TRUE)
  expect_equal(c(nrow(params), ncol(params)), c(2, 18))

  params <- model_parameters(insight::download_model("stanreg_glm_2"), centrality = "all", test = "all", dispersion = TRUE)
  expect_equal(c(nrow(params), ncol(params)), c(3, 18))

  # Mixed
  params <- model_parameters(insight::download_model("stanreg_lmerMod_1"), centrality = "all", test = "all", dispersion = TRUE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 18))
  testthat::expect_equal(params$CI_high, c(1.54338200856639, 0.532327852257708), tolerance = 1e-3)

  params <- model_parameters(insight::download_model("stanreg_merMod_1"), centrality = "all", test = "all", dispersion = TRUE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 18))

  params <- model_parameters(insight::download_model("stanreg_merMod_2"), centrality = "all", test = "all", dispersion = TRUE)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 18))

  # GAM
  params <- model_parameters(insight::download_model("stanreg_gam_1"), centrality = "all", test = "all", dispersion = TRUE)
  # testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 22)) # skip on travis and CRAN for now until new insight
})



# BRMS --------------------------------------------------------------------


test_that("model_parameters.brmsfit", {
  skip_on_travis()
  skip_on_cran()

  library(brms)
  # LM
  # testthat::expect_warning(params <- model_parameters(insight::download_model("brms_mixed_1"), standardize = "refit", centrality = "all", test = c("pd", "rope"), dispersion=TRUE))
  # testthat::expect_equal(nrow(params), 2)
  # testthat::expect_equal(ncol(params), 15)
})
