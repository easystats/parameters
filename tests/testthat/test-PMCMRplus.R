if (requiet("testthat") && requiet("parameters") && requiet("PMCMRplus")) {
  test_that("model_parameters.PMCMR", {
    set.seed(123)
    mod <- suppressWarnings(kwAllPairsConoverTest(count ~ spray, data = InsectSprays))
    df <- as.data.frame(model_parameters(mod))

    # no need to add strict tests, since `toTidy` is tested in `PMCMRplus` itself
    expect_equal(dim(df), c(15L, 8L))
  })
}
