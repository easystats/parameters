test_that("odds_to_probs", {
  testthat::expect_equal(odds_to_probs(-1.6), 2.66, tolerance = 0.01)
  testthat::expect_equal(odds_to_probs(-1.6, log = TRUE), 0.17, tolerance = 0.01)

  testthat::expect_equal(
    ncol(odds_to_probs(
      iris,
      select = c("Sepal.Length"),
      exclude = c("Petal.Length")
    )),
    5
  )
})