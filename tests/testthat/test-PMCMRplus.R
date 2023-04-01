test_that("model_parameters.PMCMR", {
  skip_if_not_installed("PMCMRplus")
  set.seed(123)
  mod <- suppressWarnings(PMCMRplus::kwAllPairsConoverTest(count ~ spray, data = InsectSprays))
  df <- as.data.frame(model_parameters(mod))

  # no need to add strict tests, since `toTidy` is tested in `PMCMRplus` itself
  expect_equal(dim(df), c(15L, 8L))
})

