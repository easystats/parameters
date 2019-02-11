context("model_parameters.lm")

test_that("model_parameters.lm", {
  model <- lme4::lmer(mpg ~ wt + (1|gear), data = mtcars)

  params <- model_parameters(model, standardize = TRUE)
  testthat::expect_equal(nrow(params), 2)
  testthat::expect_equal(ncol(params), 6)

  # TODO: Not sure how to deal with bootstrapped mixed models... As it throws a reasonable amount of singular fits...

  # params <- model_parameters(model, standardize = TRUE, bootstrap = TRUE, estimate = c("median", "mean", "MAP"), test = c("pd", "rope", "p_map", n=200, silent=TRUE))
  # testthat::expect_equal(nrow(params), 2)
  # testthat::expect_equal(ncol(params), 19)

})
