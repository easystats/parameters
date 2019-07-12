context("n_factors")

test_that("n_factors", {
  set.seed(333)
  x <- n_factors(mtcars[, 1:4])
  testthat::expect_equal(c(nrow(x), ncol(x)), c(10, 3))
})
