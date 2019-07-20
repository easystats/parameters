context("format")

test_that("format_ci", {
  testthat::expect_equal(nchar(format_ci(1.2012313, 145)), 21)
})


test_that("format_p", {
  testthat::expect_equal(nchar(format_p(0.02)), 5)
  testthat::expect_equal(nchar(format_p(0.02, stars = TRUE)), 6)
  testthat::expect_equal(nchar(format_p(0.02, stars_only = TRUE)), 1)
})

test_that("format_value", {
  testthat::expect_equal(nchar(format_value(1.2012313)), 4)
  testthat::expect_true(is.int(2))
  testthat::expect_equal(nchar(format_value(4.2, protect_integers = TRUE)), 4)
  testthat::expect_equal(nchar(format_value(4.0, protect_integers = TRUE)), 1)
})

test_that("format others", {
  testthat::expect_true(is.character(format_pd(0.02)))
  testthat::expect_equal(nchar(format_bf(4)), 9)
  testthat::expect_true(is.character(format_rope(0.02)))
})
