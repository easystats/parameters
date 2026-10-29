skip_if_not_installed("datawizard")

test_that("standard_error returns the SE column of skewness and kurtosis output", {
  out <- datawizard::skewness(mtcars[c("mpg", "cyl", "disp")])
  expect_identical(standard_error(out), out$SE)
  out <- datawizard::kurtosis(mtcars$mpg)
  expect_identical(standard_error(out), out$SE)
})

test_that("standard_error works for the datawizard_* classes", {
  s <- datawizard::skewness(mtcars$mpg)
  class(s) <- c("datawizard_skewness", "data.frame")
  expect_identical(standard_error(s), s$SE)

  k <- datawizard::kurtosis(mtcars$mpg)
  class(k) <- c("datawizard_kurtosis", "data.frame")
  expect_identical(standard_error(k), k$SE)
})

test_that("standard_error reads the SE attribute of old objects", {
  s <- 0.67
  attr(s, "SE") <- 0.39
  class(s) <- c("parameters_skewness", "numeric")
  expect_identical(standard_error(s), 0.39)
})
