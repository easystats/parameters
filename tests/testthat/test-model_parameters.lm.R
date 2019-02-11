context("model_parameters.lm")

test_that("model_parameters.lm", {
  model <- lm(mpg ~ wt + cyl, data = mtcars)

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 12)

  params <- model_parameters(model, standardize = TRUE, bootstrap = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map", n = 500))
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 19)
})
