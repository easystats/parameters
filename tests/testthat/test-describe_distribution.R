context("describe_distribution")

test_that("describe_distribution", {
  x <- describe_distribution(rnorm(100))
  testthat::expect_equal(c(nrow(x), ncol(x)), c(1, 8))
})
