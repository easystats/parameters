test_that("multivariate used to break: Insight Issue #618", {
  skip_if_not_installed("estimatr")

  # multivariate
  mod1 <- estimatr::lm_robust(cbind(mpg, qsec) ~ cyl + disp, data = mtcars)
  m <- model_parameters(mod1)
  expect_s3_class(m, "parameters_model")

  # univariate
  mod2 <- estimatr::lm_robust(mpg ~ cyl + disp, data = mtcars)
  m <- model_parameters(mod2)
  expect_s3_class(m, "parameters_model")
})
