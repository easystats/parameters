test_that("model_parameters.data.frame", {
  data(iris)
  expect_warning(expect_null(model_parameters(iris)))
})

test_that("model_parameters.data.frame as draws", {
  data(iris)
  mp <- suppressWarnings(model_parameters(iris[1:4], as_draws = TRUE))
  expect_equal(mp$Median, c(5.8, 3, 4.35, 1.3), tolerance = 1e-2, ignore_attr = TRUE)
  expect_identical(mp$Parameter, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
  expect_identical(colnames(mp), c("Parameter", "Median", "CI_low", "CI_high", "pd"))
})

# require model input
test_that("model_parameters", {
  expect_error(model_parameters())
})
