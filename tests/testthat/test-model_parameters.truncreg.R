if (require("testthat") && require("truncreg") && require("survival") && require("parameters")) {
  set.seed(123)
  data("tobin", package = "survival")

  model <- truncreg(
    formula = durable ~ age + quant,
    data = tobin,
    subset = durable > 0
  )

  test_that("model_parameters.truncreg", {
    params <- model_parameters(model)
    testthat::expect_equal(params$SE, c(9.21875, 0.22722, 0.03259, 0.56841), tolerance = 1e-3)
    testthat::expect_equal(params$t, c(1.36653, 1.89693, -3.64473, 2.90599), tolerance = 1e-3)
    testthat::expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "t",  "df_error", "p")
    )
  })
}
