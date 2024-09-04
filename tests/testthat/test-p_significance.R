skip_on_cran()
skip_if_not_installed("bayestestR", minimum_version = "0.14.1")

test_that("p_significance", {
  data(mtcars)
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  set.seed(123)
  x <- p_significance(m)
  expect_identical(c(nrow(x), ncol(x)), c(5L, 5L))
  expect_named(x, c("Parameter", "CI", "CI_low", "CI_high", "ps"))
  expect_snapshot(print(x))

  set.seed(123)
  x <- p_significance(m, ci = 0.8)
  expect_equal(x$ps, c(1, 0.4061, 0.9975, 0.6229, 0), tolerance = 1e-4)

  set.seed(123)
  x <- p_significance(m, threshold = 0.5)
  expect_equal(x$ps, c(1, 0.4471, 0.998, 0.676, 0), tolerance = 1e-4)
})
