if (require("testthat") && require("parameters")) {
  test_that("skewness", {
    data(iris)
    testthat::expect_equal(skewness(iris$Sepal.Length)[[1]], 0.314911, tol = 1e-3)
    testthat::expect_equal(skewness(iris$Sepal.Length, type = 1)[[1]], 0.3117531, tol = 1e-3)
    testthat::expect_equal(skewness(iris$Sepal.Length, type = 3)[[1]], 0.3086407, tol = 1e-3)
  })
  test_that("kurtosis", {
    data(iris)
    testthat::expect_equal(kurtosis(iris$Sepal.Length)[[1]], -0.552064, tol = 1e-3)
    testthat::expect_equal(kurtosis(iris$Sepal.Length, type = 1)[[1]], -0.5735679, tol = 1e-3)
    testthat::expect_equal(kurtosis(iris$Sepal.Length, type = 3)[[1]], -0.6058125, tol = 1e-3)
  })
  test_that("skewness", {
    data(iris)
    testthat::expect_equal(
      skewness(iris[, 1:4])[[2]],
      c(
        0.314910956636973, 0.318965664713603,
        -0.274884179751012, -0.10296674764898
      ),
      tol = 1e-3
    )
  })
  test_that("kurtosis", {
    data(iris)
    testthat::expect_equal(
      kurtosis(iris[, 1:4])[[2]],
      c(
        -0.552064041315639, 0.228249042468194,
        -1.40210341552175, -1.34060399661265
      ),
      tol = 1e-3
    )
  })
}
