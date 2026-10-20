skip_if_not_installed("datawizard")

test_that("standard_error returns the SE of skewness and kurtosis output", {
  set.seed(123)
  inputs <- list(
    vector = function(f) f(mtcars$mpg),
    bootstrap = function(f) f(mtcars$mpg, iterations = 50),
    data_frame = function(f) f(mtcars[1:3])
  )
  for (f in list(datawizard::skewness, datawizard::kurtosis)) {
    for (input in inputs) {
      out <- input(f)
      se <- standard_error(out)
      expect_type(se, "double")
      expect_null(names(se))
      expect_identical(se, out$SE)
    }
  }
  expect_length(standard_error(datawizard::skewness(mtcars[1:3])), 3)
})

test_that("standard_error works for the datawizard class names", {
  s <- datawizard::skewness(mtcars$mpg)
  class(s) <- c("datawizard_skewness", "data.frame")
  expect_identical(standard_error(s), s$SE)

  k <- datawizard::kurtosis(mtcars$mpg)
  class(k) <- c("datawizard_kurtosis", "data.frame")
  expect_identical(standard_error(k), k$SE)
})

test_that("standard_error reads the SE attribute of old objects", {
  s <- structure(0.67, SE = 0.39, class = c("parameters_skewness", "numeric"))
  expect_identical(standard_error(s), 0.39)

  k <- structure(0.67, SE = 0.39, class = c("parameters_kurtosis", "numeric"))
  expect_identical(standard_error(k), 0.39)
})
