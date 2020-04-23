if (require("testthat") && require("insight") && require("parameters") && require("mgcv")) {
  set.seed(123)
  model <-
    mgcv::gam(
      formula = mpg ~ s(hp) + s(wt) + factor(cyl) + am + qsec,
      family = stats::quasi(),
      data = mtcars
    )

  test_that("model_parameters.gam", {
    params <- model_parameters(model)
    testthat::expect_equal(params$SE, c(10.83359, 1.80704, 2.82608, 1.71366, 0.53172, NA, NA), tolerance = 1e-2)
    testthat::expect_equal(params$df_error, c(23.3923, 23.3923, 23.3923, 23.3923, 23.3923, 1.7302, 2.6904), tolerance = 1e-2)
    testthat::expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "t / F",
        "df_error", "p", "Component")
    )
  })
}
