if (requiet("testthat") && requiet("parameters") && getRversion() >= "3.6.0") {
  set.seed(123)
  mod <- lm(formula = cbind(mpg, disp) ~ wt, data = mtcars)
  mp <- model_parameters(mod)

  test_that("model_parameters,mlm", {
    expect_equal(
      mp$Coefficient,
      c(37.28513, -5.34447, -131.14842, 112.47814),
      tolerance = 1e-3
    )
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t",
        "df_error", "p", "Response"
      )
    )
    expect_equal(mp$Response, c("mpg", "mpg", "disp", "disp"))
    expect_equal(mp$Parameter, c("(Intercept)", "wt", "(Intercept)", "wt"))
  })

  model <- lm(cbind(mpg, hp) ~ cyl * disp, mtcars)
  mp <- model_parameters(model)

  test_that("model_parameters,mlm", {
    expect_equal(
      mp$Coefficient,
      c(49.03721, -3.40524, -0.14553, 0.01585, 23.55, 17.43527, -0.36762, 0.06174),
      tolerance = 1e-3
    )
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t",
        "df_error", "p", "Response"
      )
    )
    expect_equal(mp$Response, c("mpg", "mpg", "mpg", "mpg", "hp", "hp", "hp", "hp"))
    expect_equal(mp$Parameter, c("(Intercept)", "cyl", "disp", "cyl:disp", "(Intercept)", "cyl", "disp", "cyl:disp"))
  })
}
