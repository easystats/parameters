test_that("model_parameters.Arima", {
  model <- stats::arima(datasets::lh, order = c(1, 0, 0))
  params <- model_parameters(model)
  expect_identical(params$Parameter, c("ar1", "intercept"))
  expect_identical(attr(params, "pretty_labels"), c(ar1 = "ar1", intercept = "intercept"))
})
