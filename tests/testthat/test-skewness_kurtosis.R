skip_if_not_installed("datawizard")

test_that("standard_error returns the SE of skewness and kurtosis output", {
  inputs <- list(
    vector = function(f) f(mtcars$mpg),
    bootstrap = function(f) f(mtcars$mpg, iterations = 50L),
    data_frame = function(f) f(mtcars[c("mpg", "cyl", "disp")])
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
  out <- datawizard::skewness(mtcars[c("mpg", "cyl", "disp")])
  expect_length(standard_error(out), 3L)

  # known values, independent of how datawizard computes them
  x <- data.frame(
    Parameter = c("a", "b"),
    Skewness = c(0.5, 1.5),
    SE = c(0.25, 0.3),
    stringsAsFactors = FALSE
  )
  class(x) <- c("parameters_skewness", "data.frame")
  expect_identical(standard_error(x), c(0.25, 0.3))
  class(x) <- c("parameters_kurtosis", "data.frame")
  expect_identical(standard_error(x), c(0.25, 0.3))
})

# datawizard after 1.4.1 adds the classes "datawizard_skewness" and
# "datawizard_kurtosis" before the "parameters_*" classes
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

  class(s) <- c("parameters_kurtosis", "numeric")
  expect_identical(standard_error(s), 0.39)
})
