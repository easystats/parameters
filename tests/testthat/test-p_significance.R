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
  expect_equal(x$ps, c(1, 0.4074, 0.9972, 0.6212, 0), tolerance = 1e-4)

  set.seed(123)
  x <- p_significance(m, threshold = 0.5)
  expect_equal(x$ps, c(1, 0.4478, 0.9977, 0.6737, 0), tolerance = 1e-4)
})

test_that("p_significance, glmmTMB", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(count ~ mined + cover + (1 | site),
    zi = ~mined,
    family = poisson,
    data = Salamanders
  )
  out <- p_significance(m1)
  expect_identical(c(nrow(out), ncol(out)), c(5L, 6L))
  expect_named(out, c("Parameter", "CI", "CI_low", "CI_high", "ps", "Component"))
  expect_equal(out$ps, c(0.6451, 1, 0.9015, 1, 1), tolerance = 1e-4)
  expect_identical(
    out$Parameter,
    c(
      "(Intercept)_cond", "minedno_cond", "cover_cond", "(Intercept)_zi",
      "minedno_zi"
    )
  )
})
