context("model_parameters.glm")

test_that("model_parameters.glm - binomial", {
  set.seed(333)

  model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

  params <- model_parameters(model, standardize = "refit")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 9))


  params <- suppressWarnings(model_parameters(model, standardize = "refit", estimate = "all", test = "all", dispersion=TRUE, bootstrap = TRUE, n = 500))
  testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 13))
})
