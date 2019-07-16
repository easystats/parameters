context("convert")

test_that("odds_to_probs", {
  testthat::expect_equal(odds_to_probs(-1.6), 2.66, tolerance = 0.01)
  testthat::expect_equal(odds_to_probs(-1.6, log = TRUE), 0.17, tolerance = 0.01)
  testthat::expect_equal(probs_to_odds(2.66), -1.6, tolerance = 0.01)
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

test_that("odds_to_d", {
  testthat::expect_equal(odds_to_d(0.2), -0.887, tolerance = 0.01)
  testthat::expect_equal(odds_to_d(-1.45, log = TRUE), -0.7994, tolerance = 0.01)
  testthat::expect_equal(d_to_odds(-0.887), 0.2, tolerance = 0.01)
  testthat::expect_equal(d_to_odds(-0.7994, log = TRUE), -1.45, tolerance = 0.01)
})


test_that("d_to_r", {
  testthat::expect_equal(d_to_r(d = 1.1547), 0.5, tolerance = 0.01)
  testthat::expect_equal(r_to_d(r = 0.5), 1.1547, tolerance = 0.01)
})
