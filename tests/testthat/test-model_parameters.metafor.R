test_that("model_parameters.metafor", {
  skip_if_not_installed("metafor")
  test <- data.frame(
    estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
    std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
  )
  mydat <<- test
  model <- metafor::rma(yi = estimate, sei = std.error, data = mydat)
  params <- model_parameters(model)

  expect_identical(
    params$Parameter,
    c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5", "Overall")
  )
  expect_identical(
    colnames(params),
    c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "z", "p", "Weight", "Method")
  )
  expect_equal(params$Coefficient, c(0.111, 0.245, 0.8, 1.1, 0.03, 0.43769), tolerance = 1e-3)
  expect_equal(params$Weight, c(400, 81.16224, 1e+06, 25, 10000, NA), tolerance = 1e-3)
  # test message on unsupported arguments
  expect_message(model_parameters(model, vcov = "vcovHC"), regex = "Following arguments")
  # test standardize
  params <- model_parameters(model, standardize = "refit")
  expect_equal(params$Coefficient, c(0.111, 0.245, 0.8, 1.1, 0.03, -0.5613041), tolerance = 1e-3)
})
