context("format")

test_that("format_ci", {
  testthat::expect_equal(nchar(format_ci(1.2012313, 145)), 21)
})


test_that("format_p", {
  testthat::expect_equal(nchar(format_p(0.02)), 7)
  testthat::expect_equal(nchar(format_p(0.02, stars = TRUE)), 8)
  testthat::expect_equal(nchar(format_p(0.02, stars_only = TRUE)), 1)
})

test_that("format_value", {
  testthat::expect_equal(nchar(format_value(1.2012313)), 4)
  testthat::expect_true(is.int(2))
  testthat::expect_equal(nchar(format_value(4.2, protect_integers = TRUE)), 4)
  testthat::expect_equal(nchar(format_value(4.0, protect_integers = TRUE)), 1)
})

test_that("format_number and format_order", {
  testthat::expect_equal(format_number(2), "two")
  testthat::expect_equal(format_number(45), "forty five")
  testthat::expect_equal(format_number(2), "two")

  testthat::expect_equal(format_order(2), "second")
  testthat::expect_equal(format_order(45), "forty fifth")

  testthat::expect_equal(format_order(2, textual = FALSE), "2nd")
  testthat::expect_equal(format_order(45, textual = FALSE), "45th")
})


test_that("format others", {
  testthat::expect_true(is.character(format_pd(0.02)))
  testthat::expect_equal(nchar(format_bf(4)), 9)
  testthat::expect_true(is.character(format_rope(0.02)))
})
