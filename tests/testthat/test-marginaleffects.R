skip_if_not_installed("marginaleffects", minimum_version = "0.18.0")
skip_if_not_installed("insight", minimum_version = "0.19.9")
skip_if_not_installed("rstanarm")

test_that("marginaleffects()", {
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  model <- marginaleffects::avg_slopes(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
  out <- parameters(model)
  expect_identical(nrow(out), 1L)
  expect_named(out, c(
    "Parameter", "Coefficient", "SE", "Statistic",
    "p", "S", "CI", "CI_low", "CI_high"
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
  model <- marginaleffects::avg_slopes(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
  expect_identical(nrow(parameters(model)), 1L)
})


test_that("predictions()", {
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  p <- marginaleffects::avg_predictions(x, by = "Species")
  out <- parameters(p)
  expect_identical(nrow(out), 3L)
  expect_named(out, c(
    "Predicted", "SE", "CI", "CI_low", "CI_high", "S", "Statistic",
    "p", "Species"
  ))
  out <- parameters(p, exponentiate = TRUE)
  expect_equal(out$Predicted, c(30.81495, 15.95863, 19.57004), tolerance = 1e-4)
})


test_that("comparisons()", {
  data(iris)
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  m <- marginaleffects::avg_comparisons(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")
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
  m <- marginaleffects::avg_slopes(
    x,
    newdata = insight::get_datagrid(x, at = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(parameters(m)), 1L)
})


test_that("hypotheses()", {
  data(mtcars)
  x <- lm(mpg ~ hp + wt, data = mtcars)
  m <- marginaleffects::hypotheses(x, "hp = wt")
  expect_identical(nrow(parameters(m)), 1L)
})


test_that("multiple contrasts: Issue #779", {
  skip_if(getRversion() < "4.0.0")
  data(mtcars)
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


test_that("model_parameters defaults to FALSE: Issue #916", {
  data(mtcars)
  mod <- lm(mpg ~ wt, data = mtcars)
  pred <- marginaleffects::predictions(mod, newdata = marginaleffects::datagrid(wt = c(1, 2)))
  out1 <- model_parameters(pred)
  out2 <- model_parameters(pred, exponentiate = FALSE)
  expect_equal(out1$Predicted, out2$Predicted, tolerance = 1e-4)
})
