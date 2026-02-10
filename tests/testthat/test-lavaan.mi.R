skip_on_cran()
skip_if_not_installed("lavaan.mi")
skip_if_not_installed("mice")

test_that("model_parameters.lavaan.mi", {
  # required for lavaan.mi to work
  library(lavaan.mi)

  set.seed(20260205)
  mtcars_amp <- suppressWarnings(mice::ampute(mtcars)$amp)
  imp <- mice::mice(mtcars_amp, printFlag = FALSE)

  mod <- "
  mpg ~ hp + vs
  hp ~ am
  "

  fit.mi <- lavaan.mi::sem.mi(mod, data = imp)

  mp <- model_parameters(fit.mi)
  expect_s3_class(mp, "parameters_sem")
  expect_equal(nrow(mp), 4)
  expect_equal(mp$Coefficient[1], -0.05933624, tolerance = 1e-2)
})
