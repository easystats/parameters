skip_on_cran()
skip_if_not_installed("bayestestR")
skip_if_not_installed("distributional")

test_that("p_direction", {
  data(mtcars)
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  set.seed(123)
  x <- p_direction(m)
  expect_identical(c(nrow(x), ncol(x)), c(5L, 5L))
  expect_named(x, c("Parameter", "CI", "CI_low", "CI_high", "pd"))
  expect_snapshot(print(x))

  set.seed(123)
  x <- p_direction(m, ci = 0.8)
  expect_equal(x$pd, c(1, 0.6359, 0.9992, 0.882, 0.9117), tolerance = 1e-3)

  set.seed(123)
  x <- p_direction(m, null = 0.2)
  expect_equal(x$pd, c(1, 0.5567, 0.9997, 0.9309, 1), tolerance = 1e-3)
})

test_that("p_direction", {
  skip_if_not_installed("sandwich")
  data(mtcars)
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)

  set.seed(123)
  x <- p_direction(m, ci = 0.8, vcov = "HC3")
  expect_equal(x$pd, c(1, 0.6162, 0.9984, 0.8323, 0.8962), tolerance = 1e-3)

  set.seed(123)
  x <- p_direction(m, null = 0.2, vcov = "HC3")
  expect_equal(x$pd, c(1, 0.5464, 0.9989, 0.88, 1), tolerance = 1e-3)
})

test_that("p_direction, glmmTMB", {
  skip_if_not_installed("glmmTMB")
  data(Salamanders, package = "glmmTMB")
  m1 <- glmmTMB::glmmTMB(count ~ mined + cover + (1 | site),
    zi = ~mined,
    family = poisson,
    data = Salamanders
  )
  out <- p_direction(m1)
  expect_identical(c(nrow(out), ncol(out)), c(5L, 6L))
  expect_named(out, c("Parameter", "CI", "CI_low", "CI_high", "pd", "Component"))
  expect_equal(out$pd, c(0.8245, 1, 0.9974, 1, 1), tolerance = 1e-4)
  expect_identical(
    out$Parameter,
    c(
      "(Intercept)_cond", "minedno_cond", "cover_cond", "(Intercept)_zi",
      "minedno_zi"
    )
  )
})
