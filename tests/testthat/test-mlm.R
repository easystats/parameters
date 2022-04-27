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

  test_that("get_varcov sandwich", {
    requiet("sandwich")
    mod <- lm(formula = cbind(mpg, disp) ~ wt + factor(cyl) + am, data = mtcars)
    se1 <- standard_error(mod)
    se2 <- standard_error(mod, vcov = "HC3")
    se3 <- standard_error(mod, vcov = sandwich::vcovHC)
    se4 <- sqrt(diag(sandwich::vcovHC(mod)))
    expect_true(all(se1$SE != se2$SE))
    expect_true(all(se2$SE == se3$SE))
    expect_true(all(se2$SE == se4))
    lab <- strsplit(names(se4), ":")
    expect_equal(se2$Parameter, sapply(lab, function(x) x[2]))
    expect_equal(se2$Response, sapply(lab, function(x) x[1]))
  })

}
