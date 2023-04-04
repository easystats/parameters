test_that("model_parameters.cpglmm", {
  # 'cplm' exports a class "mcmc" which conflicts with the one of 'BayesFactor'
  # (in the tests with random order).
  skip_if(requireNamespace("BayesFactor", quietly = TRUE))
  skip_if_not_installed("cplm")

  data("FineRoot", package = "cplm")
  cpglmm <- cplm::cpglmm

  model <- cpglmm(RLD ~ Stock + Spacing + (1 | Plant), data = FineRoot)

  params <- model_parameters(model, effects = "fixed")
  expect_equal(params$SE, c(0.1308, 0.2514, 0.2, 0.1921), tolerance = 1e-3)
  expect_equal(
    colnames(params),
    c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Effects")
  )
})
