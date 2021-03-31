test_that("model_parameters output as expected", {
  skip_if(getRversion() < "3.5")
  set.seed(123)
  mod <- lm(formula = wt ~ am * cyl, data = mtcars)
  expect_snapshot(as.data.frame(parameters(mod)))
})