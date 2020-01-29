if (require("testthat") && require("bayestestR") && require("parameters")) {
  test_that("distributions", {
    x <- bayestestR::distribution_normal(100)

    testthat::expect_equal(kurtosis(x), -0.3204763, tol = 0.01)
    testthat::expect_equal(skewness(x), -5.050428e-16, tol = 0.01)
    testthat::expect_equal(smoothness(x, "diff"), 0.919, tol = 0.01)
    testthat::expect_equal(smoothness(x, "cor"), 0.998, tol = 0.01)
  })
}
