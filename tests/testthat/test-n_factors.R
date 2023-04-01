test_that("n_factors", {
  skip_if_not_installed("nFactors")
  skip_if_not_installed("psych")
  set.seed(333)
  x <- n_factors(mtcars[, 1:4])
  expect_equal(ncol(x), 3)
})
