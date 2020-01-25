if (require("testthat") && require("insight") && require("parameters") && require("metafor")) {
  data <- data.frame(
    estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
    std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
  )
  model <- metafor::rma(yi = estimate, sei = std.error, data = data)
  params <- model_parameters(model)

  test_that("model_parameters.cpglmm", {
    expect_equal(params$Parameter, c("Study 1", "Study 2", "Study 3", "Study 4", "Study 5", "Overall"))
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "z", "p", "Weight")
    )
    expect_equal(params$Coefficient, c(0.111, 0.245, 0.8, 1.1, 0.03, 0.43769), tolerance = 1e-3)
    expect_equal(params$Coefficient, c(400, 81.16224, 1e+06, 25, 10000, NA), tolerance = 1e-3)
  })
}
