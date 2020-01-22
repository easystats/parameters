if (require("testthat") && require("parameters")) {
  test_that("skewness", {
    data(iris)
    testthat::expect_equal(skewness(iris$Sepal.Length), 0.314911, tol = 1e-3)
    testthat::expect_equal(skewness(iris$Sepal.Length, type = 1), 0.3117531, tol = 1e-3)
    testthat::expect_equal(skewness(iris$Sepal.Length, type = 3), 0.3086407, tol = 1e-3)
  })
  test_that("kurtosis", {
    data(iris)
    testthat::expect_equal(kurtosis(iris$Sepal.Length), -0.552064, tol = 1e-3)
    testthat::expect_equal(kurtosis(iris$Sepal.Length, type = 1), -0.5735679, tol = 1e-3)
    testthat::expect_equal(kurtosis(iris$Sepal.Length, type = 3), -0.6058125, tol = 1e-3)
  })
  test_that("skewness", {
    data(iris)
    testthat::expect_equal(
      skewness(iris[, 1:4]),
      c(Sepal.Length = 0.314910956636973, Sepal.Width = 0.318965664713603,
        Petal.Length = -0.274884179751012, Petal.Width = -0.10296674764898),
      tol = 1e-3
    )
  })
  test_that("kurtosis", {
    data(iris)
    testthat::expect_equal(
      kurtosis(iris[, 1:4]),
      c(Sepal.Length = -0.552064041315639, Sepal.Width = 0.228249042468194,
        Petal.Length = -1.40210341552175, Petal.Width = -1.34060399661265),
      tol = 1e-3
    )
  })
}
