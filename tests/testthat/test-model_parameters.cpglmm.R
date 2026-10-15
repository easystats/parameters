unloadNamespace("BayesFactor")

test_that("model_parameters.cpglmm", {
  skip_if_not_installed("cplm")
  loadNamespace("cplm")

  data("FineRoot", package = "cplm")
  cpglmm <- cplm::cpglmm

  model <- cpglmm(RLD ~ Stock + Spacing + (1 | Plant), data = FineRoot)

  params <- model_parameters(model, effects = "fixed")
  expect_equal(params$SE, c(0.1308, 0.2514, 0.2, 0.1921), tolerance = 1e-3)
  expect_equal(
    colnames(params),
    c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Effects")
  )

  unloadNamespace("cplm")
})

test_that("bootstrap_model resamples clusters for cpglmm", {
  skip_on_cran()
  skip_if_not_installed("cplm")
  skip_if_not_installed("boot")
  skip_if_not_installed("withr")
  # cplm's S4 class "mcmc" masks the one from BayesFactor, so cplm is unloaded
  # again, and this test stays in a file that runs after the BayesFactor tests
  withr::defer(unloadNamespace("cplm"))
  # update() needs cpglmm() on the search path
  withr::local_package("cplm")
  data("FineRoot", package = "cplm")
  m <- cpglmm(RLD ~ Stock + Spacing + (1 | Plant), data = FineRoot)
  .expect_cluster_bootstrap(m, FineRoot, "Plant")
})
