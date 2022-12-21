requiet("mice")

data("nhanes2")
set.seed(123)
imp <- mice(nhanes2, printFlag = FALSE)
models <- lapply(1:5, function(i) {
  lm(bmi ~ age + hyp + chl, data = complete(imp, action = i))
})
pp <- pool_parameters(models)

test_that("pooled parameters", {
  expect_equal(pp$df_error, c(9.2225, 8.1903, 3.6727, 10.264, 6.4385), tolerance = 1e-3)
  expect_snapshot(print(pp))
})
