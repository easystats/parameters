context("format")

test_that("format_ci", {
  testthat::expect_equal(nchar(format_ci(1.2012313, 145)), 21, tol = 0)
})


test_that("format_p", {
  testthat::expect_equal(nchar(format_p(0.02)), 5, tol = 0)
  testthat::expect_equal(nchar(format_p(0.02, stars = TRUE)), 6, tol = 0)
  testthat::expect_equal(nchar(format_p(0.02, stars_only = TRUE)), 1, tol = 0)
})

test_that("format_value", {
  testthat::expect_equal(nchar(format_value(1.2012313)), 4, tol = 0)
})
