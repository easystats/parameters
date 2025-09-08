skip_on_cran()
skip_if_not_installed("bayestestR")
skip_if_not_installed("distributional")
skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("p_significance", {
    data(mtcars)
    m <<- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
    set.seed(123)
    x <- p_significance(m)
    expect_identical(c(nrow(x), ncol(x)), c(5L, 5L))
    expect_named(x, c("Parameter", "CI", "CI_low", "CI_high", "ps"))
    expect_snapshot(print(x))

    mp <- model_parameters(m)
    set.seed(123)
    x2 <- p_significance(mp)
    expect_equal(x$ps, x2$ps, tolerance = 1e-4)

    set.seed(123)
    x <- p_significance(m, ci = 0.8)
    expect_equal(x$ps, c(1, 0.3983, 0.9959, 0.6188, 0), tolerance = 1e-3)

    set.seed(123)
    x <- p_significance(m, threshold = 0.5)
    expect_equal(x$ps, c(1, 0.4393, 0.9969, 0.6803, 0), tolerance = 1e-4)

    set.seed(123)
    # Test p_significance with custom thresholds for specific parameters
    x <- p_significance(m, threshold = list(cyl = 0.5, wt = 0.7))
    expect_equal(x$ps, c(1, 0.5982, 0.9955, 0.6803, 1e-04), tolerance = 1e-4)
  })
)

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

test_that("p_significance, robust", {
  skip_if_not_installed("sandwich")
  data(mtcars)
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  set.seed(123)
  x <- p_significance(m, vcov = "HC3")
  expect_snapshot(print(x))
})
