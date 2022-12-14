if (


  requiet("mgcv")) {
  set.seed(123)
  model <-
    mgcv::gam(
      formula = mpg ~ s(hp) + s(wt) + factor(cyl) + am + qsec,
      family = stats::quasi(),
      data = mtcars
    )

  test_that("model_parameters.gam", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(10.83359, 1.80704, 2.82608, 1.71366, 0.53172, NA, NA), tolerance = 1e-2)
    expect_equal(params$df_error, c(23.3923, 23.3923, 23.3923, 23.3923, 23.3923, NA, NA), tolerance = 1e-2)
    expect_equal(params$CI[[1]], .95, tolerance = 1e-2)
    expect_equal(
      colnames(params),
      c(
        "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t / F",
        "df", "df_error", "p", "Component"
      )
    )
  })
}
