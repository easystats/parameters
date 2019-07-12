context("n_factors")

test_that("n_factors", {
  x <- n_factors(mtcars)
  testthat::expect_equal(c(nrow(x), ncol(x)), c(5, 3))
})
