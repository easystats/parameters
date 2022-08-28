requiet("estimatr")

test_that("multivariate used to break: Insight Issue #618", {
  # multivariate
  mod1 <- lm_robust(cbind(mpg, qsec) ~ cyl + disp, data = mtcars)
  m <- model_parameters(mod1)
  expect_s3_class(m, "parameters_model")

  # univariate
  mod2 <- lm_robust(mpg ~ cyl + disp, data = mtcars)
  m <- model_parameters(mod2)
  expect_s3_class(m, "parameters_model")
})
