if (require("testthat") && require("bayestestR") && require("parameters")) {
  test_that("distributions", {
    x <- bayestestR::distribution_normal(100)

    expect_equal(kurtosis(x)$Kurtosis, -0.3204763, tolerance = 0.01)
    expect_equal(skewness(x)$Skewness, -5.050428e-16, tolerance = 0.01)
    expect_equal(as.numeric(smoothness(x, "diff")), 0.919, tolerance = 0.01)
    expect_equal(as.numeric(smoothness(x, "cor")), 0.998, tolerance = 0.01)
  })
}
