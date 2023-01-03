test_that("select_parameters", {
  model <- lm(mpg ~ ., data = mtcars)
  x <- select_parameters(model)
  expect_equal(n_parameters(model) - n_parameters(x), 7)
})
