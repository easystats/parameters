skip_if_not_installed("marginaleffects", minimum_version = "0.25.0")
skip_if_not_installed("rstanarm")

test_that("marginaleffects()", {
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  model <- marginaleffects::avg_slopes(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  out <- model_parameters(model)
  expect_identical(nrow(out), 1L)
  cols <- c(
    "Parameter",
    "Comparison",
    "Coefficient",
    "SE",
    "Statistic",
    "p",
    "S",
    "CI",
    "CI_low",
    "CI_high"
  )
  expect_true(all(cols %in% colnames(out)))
  out <- model_parameters(model, exponentiate = TRUE)
  expect_equal(out$Coefficient, 1.394, tolerance = 1e-3)

  # Bayesian
  x <- suppressWarnings(rstanarm::stan_glm(
    Sepal.Width ~ Species * Petal.Length,
    data = iris,
    refresh = 0,
    iter = 100,
    chains = 1
  ))
  model <- marginaleffects::avg_slopes(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(parameters(model)), 1L)
})


test_that("predictions()", {
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  p <- marginaleffects::avg_predictions(x, by = "Species")
  out <- model_parameters(p)
  expect_identical(nrow(out), 3L)
  expect_named(out, c(
    "Predicted", "Species", "SE", "CI", "CI_low", "CI_high", "S", "Statistic",
    "df", "p"
  ))
  out <- model_parameters(p, exponentiate = TRUE)
  expect_equal(out$Predicted, c(30.81495, 15.95863, 19.57004), tolerance = 1e-4)
})


test_that("comparisons()", {
  data(iris)
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  m <- marginaleffects::avg_comparisons(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(model_parameters(m)), 1L)
  out <- model_parameters(m, exponentiate = TRUE)
  expect_equal(out$Coefficient, 1.393999, tolerance = 1e-4)

  # Bayesian
  x <- suppressWarnings(rstanarm::stan_glm(
    Sepal.Width ~ Species * Petal.Length,
    data = iris,
    refresh = 0,
    iter = 100,
    chains = 1
  ))
  m <- marginaleffects::avg_slopes(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(parameters(m)), 1L)
})


test_that("hypotheses()", {
  data(mtcars)
  x <- lm(mpg ~ hp + wt, data = mtcars)
  m <- marginaleffects::hypotheses(x, "hp = wt")
  expect_identical(nrow(model_parameters(m)), 1L)
})


test_that("multiple contrasts: Issue #779", {
  skip_if(getRversion() < "4.0.0")
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) * as.factor(cyl), data = mtcars)
  cmp <- suppressWarnings(marginaleffects::comparisons(
    mod,
    variables = c("gear", "cyl"),
    newdata = insight::get_datagrid(mod, by = c("gear", "cyl")),
    cross = TRUE
  ))
  cmp <- suppressWarnings(model_parameters(cmp))
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


test_that("digits and ci_digits for marginaleffects", {
  data(mtcars)
  skip_if(getRversion() < "4.2.0")
  out <- lm(mpg ~ wt, data = mtcars) |>
    marginaleffects::hypotheses(hypothesis = "10*wt = 0") |>
    model_parameters(digits = 1)
  expect_snapshot(out)
})


test_that("preserve columns with same name as reserved words", {
  data(mtcars)
  skip_if(getRversion() < "4.2.0")
  skip_if_not_installed("modelbased")

  set.seed(1234)
  x <- rnorm(200)
  z <- rnorm(200)
  # quadratic relationship
  y <- 2 * x + x^2 + 4 * z + rnorm(200)
  d <- data.frame(x, y, z)
  model <- lm(y ~ x + z, data = d)
  pred <- modelbased::estimate_means(model, c("x", "z"))
  expect_named(pred, c("x", "z", "Mean", "SE", "CI_low", "CI_high", "t", "df"))
})


test_that("predictions, bmrs with special response formula", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")

  m <- insight::download_model("brms_ipw_1")
  skip_if(is.null(m))

  x <- marginaleffects::avg_predictions(m, variables = "treatment", hypothesis = ~pairwise)
  out <- model_parameters(x)
  expect_identical(dim(out), c(1L, 10L))
})


## TODO: run check manually every now and then

test_that("predictions, using bayestestR #1063", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")

  m <- insight::download_model("brms_mixed_3")
  skip_if(is.null(m))

  d <- insight::get_datagrid(m, by = "Days", include_random = TRUE)
  x <- marginaleffects::avg_predictions(m, newdata = d, by = "Days")
  out <- model_parameters(x)
  expect_named(
    out,
    c(
      "Median", "CI", "CI_low", "CI_high", "pd", "ROPE_CI", "ROPE_low",
      "ROPE_high", "ROPE_Percentage", "Days", "subgrp", "grp", "Subject"
    )
  )
})
