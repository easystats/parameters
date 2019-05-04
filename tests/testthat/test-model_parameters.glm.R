context("model_parameters.glm")

test_that("model_parameters.glm - binomial", {
  set.seed(333)

  model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

  params <- model_parameters(model, standardize = "refit")
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 9)


  params <- suppressWarnings(model_parameters(model, standardize = "refit", bootstrap = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map"), n = 500))
  testthat::expect_equal(nrow(params), 3)
  testthat::expect_equal(ncol(params), 13)
})
