skip_if_not_installed("marginaleffects", minimum_version = "0.9.0")
skip_if_not_installed("rstanarm")

test_that("marginaleffects()", {
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  model <- marginaleffects::slopes(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
  out <- parameters(model)
  expect_identical(nrow(out), 1L)
  expect_named(out, c(
    "Parameter", "Comparison", "Coefficient", "SE", "Statistic",
    "p", "CI", "CI_low", "CI_high"
  ))
  out <- model_parameters(model, exponentiate = TRUE)
  expect_equal(out$Coefficient, 1.394, tolerance = 1e-3)

  # Bayesian
  x <- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Width ~ Species * Petal.Length,
      data = iris,
      refresh = 0,
      iter = 100,
      chains = 1
    )
  )
  model <- marginaleffects::slopes(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
  expect_identical(nrow(parameters(model)), 1L)
})


test_that("predictions()", {
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  p <- marginaleffects::avg_predictions(x, by = "Species")
  out <- parameters(p)
  expect_identical(nrow(out), 3L)
  expect_named(out, c(
    "Predicted", "SE", "CI", "CI_low", "CI_high", "Statistic",
    "p", "Species", "s.value"
  ))
  out <- parameters(p, exponentiate = TRUE)
  expect_equal(out$Predicted, c(30.81495, 15.95863, 19.57004), tolerance = 1e-4)
})


test_that("comparisons()", {
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  m <- marginaleffects::comparisons(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
  expect_identical(nrow(parameters(m)), 1L)
  out <- parameters(m, exponentiate = TRUE)
  expect_equal(out$Coefficient, 1.393999, tolerance = 1e-4)

  # Bayesian
  x <- suppressWarnings(
    rstanarm::stan_glm(
      Sepal.Width ~ Species * Petal.Length,
      data = iris,
      refresh = 0,
      iter = 100,
      chains = 1
    )
  )
  m <- marginaleffects::marginaleffects(
    x,
    newdata = insight::get_datagrid(x, at = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(parameters(m)), 1L)
})


test_that("marginalmeans()", {
  dat <- mtcars
  dat$cyl <- factor(dat$cyl)
  dat$gear <- factor(dat$gear)
  x <- lm(mpg ~ cyl + gear, data = dat)
  m <- marginaleffects::marginalmeans(x)
  expect_identical(nrow(parameters(m)), 6L)
})


test_that("hypotheses()", {
  x <- lm(mpg ~ hp + wt, data = mtcars)
  m <- marginaleffects::hypotheses(x, "hp = wt")
  expect_identical(nrow(parameters(m)), 1L)
})


test_that("multiple contrasts: Issue #779", {
  skip_if(getRversion() < "4.0.0")
  mod <- lm(mpg ~ as.factor(gear) * as.factor(cyl), data = mtcars)
  cmp <- suppressWarnings(marginaleffects::comparisons(
    mod,
    variables = c("gear", "cyl"),
    newdata = insight::get_datagrid(mod, at = c("gear", "cyl")),
    cross = TRUE
  ))
  cmp <- suppressWarnings(parameters(cmp))
  expect_true("Comparison: gear" %in% colnames(cmp))
  expect_true("Comparison: cyl" %in% colnames(cmp))
})
