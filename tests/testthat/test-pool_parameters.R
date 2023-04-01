test_that("pooled parameters", {
  skip_if_not_installed("mice")
  data("nhanes2", package = "mice")
  set.seed(123)
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  models <- lapply(1:5, function(i) {
    lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  })
  pp <- pool_parameters(models)
  expect_equal(pp$df_error, c(9.2225, 8.1903, 3.6727, 10.264, 6.4385), tolerance = 1e-3)
  expect_snapshot(print(pp))
})
