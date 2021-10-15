if (requiet("testthat") && requiet("insight") && requiet("parameters") && requiet("MASS")) {
  model <- MASS::rlm(formula = mpg ~ am * cyl, data = mtcars)
  s <- summary(model)

  test_that("model_parameters.rlm", {
    params <- model_parameters(model)
    expect_equal(params$SE, as.vector(coef(s)[, 2]), tolerance = 1e-3)
    expect_equal(params$Coefficient, as.vector(coef(s)[, 1]), tolerance = 1e-3)
    expect_equal(params$t, as.vector(coef(s)[, 3]), tolerance = 1e-3)
    expect_equal(params$df_error, c(28, 28, 28, 28), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p")
    )
  })
}
