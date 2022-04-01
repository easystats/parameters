test_that("model_parameters.data.frame", {
  data(iris)
  expect_null(model_parameters(iris))
})
