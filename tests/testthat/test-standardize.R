context("standardize")

test_that("standardize.numeric", {
  x <- standardize(seq(0, 1, length.out = 100))
  testthat::expect_equal(mean(0), 0, tol = 0.01)

  x <- standardize(seq(0, 1, length.out = 100), robust = TRUE)
  testthat::expect_equal(median(0), 0, tol = 0.01)

  testthat::expect_message(standardize(c(0, 0, 0, 1, 1)))
})


test_that("standardize.data.frame", {
  x <- standardize(iris)
  testthat::expect_equal(mean(x$Sepal.Length), 0, tol = 0.01)
  testthat::expect_length(levels(x$Species), 3)
  testthat::expect_equal(mean(dplyr::filter_(x, "Species == 'virginica'")$Sepal.Length), 0.89, tol = 0.01)

  x <- standardize(dplyr::group_by_(iris, "Species"))
  testthat::expect_equal(mean(x$Sepal.Length), 0, tol = 0.01)
  testthat::expect_length(levels(x$Species), 3)
  testthat::expect_equal(mean(dplyr::filter_(x, "Species == 'virginica'")$Sepal.Length), 0, tol = 0.01)
})

test_that("normalize.data.frame", {
  x <- normalize(iris)
  testthat::expect_equal(mean(x$Sepal.Length), 0.42, tol = 0.01)
  testthat::expect_length(levels(x$Species), 3)
  testthat::expect_equal(mean(dplyr::filter_(x, "Species == 'virginica'")$Sepal.Length), 0.635, tol = 0.01)

  x <- normalize(dplyr::group_by_(iris, "Species"))
  testthat::expect_equal(mean(x$Sepal.Length), 0.509, tol = 0.01)
  testthat::expect_length(levels(x$Species), 3)
  testthat::expect_equal(mean(dplyr::filter_(x, "Species == 'virginica'")$Sepal.Length), 0.562, tol = 0.01)
})


test_that("standardize.lm", {
  model <- standardize(lm(Sepal.Length ~ Species * Petal.Width, data = iris))
  testthat::expect_equal(coef(model)[[1]], 0.059, tol = 0.01)
})
