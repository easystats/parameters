context("distributions")

test_that("distributions", {
  x <- bayestestR::rnorm_perfect(100)

  testthat::expect_equal(kurtosis(x), 2.635, tol = 0.01)
  testthat::expect_equal(skewness(x), 0, tol = 0.01)
  testthat::expect_equal(smoothness(x, "diff"), 0.919, tol = 0.01)
  testthat::expect_equal(smoothness(x, "cor"), 0.998, tol = 0.01)
  testthat::expect_is(find_distribution(x), "character")
  testthat::expect_is(find_distribution(x, probabilities = TRUE), "data.frame")
})
