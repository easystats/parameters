context("model_parameters_labels")
library(insight)
library(testthat)

test_that("model_parameters_labels", {
  library(lme4)
  library(parameters)

  data(mtcars)
  mtcars$am <- as.factor(mtcars$am)

  m1 <- lmer(mpg ~ hp * am + (1 | cyl), data = mtcars)
  expect_equal(
    attr(model_parameters(m1), "pretty_names"),
    c(`(Intercept)` = "(Intercept)", hp = "hp", am1 = "am [1]", `hp:am1` = "hp * am [1]")
  )

  m2 <- lmer(mpg ~ hp * as.factor(am) + (1 | cyl), data = mtcars)
  expect_equal(
    attr(model_parameters(m2), "pretty_names"),
    c(
      `(Intercept)` = "(Intercept)", hp = "hp", `as.factor(am)1` = "am [1]",
      `hp:as.factor(am)1` = "hp * am [1]"
    )
  )

  m3 <- lmer(mpg ~ hp * log(gear) + (1 | cyl), data = mtcars)
  expect_equal(
    attr(model_parameters(m3), "pretty_names"),
    c(
      `(Intercept)` = "(Intercept)", hp = "hp", `log(gear)` = "gear [log]",
      `hp:log(gear)` = "hp * gear [log]"
    )
  )

  m4 <- lm(mpg ~ as.factor(cyl) + hp * log(gear), data = mtcars)
  expect_equal(
    attr(model_parameters(m4), "pretty_names"),
    c(
      `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl [6]",
      `as.factor(cyl)8` = "cyl [8]", hp = "hp", `log(gear)` = "gear [log]",
      `hp:log(gear)` = "hp * gear [log]"
    )
  )

  m5 <- lm(mpg ~ as.factor(cyl) * I(wt / 10) + hp * log(gear), data = mtcars)
  expect_equal(
    attr(model_parameters(m5), "pretty_names"),
    c(
      `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl [6]",
      `as.factor(cyl)8` = "cyl [8]", `I(wt/10)` = "wt/10", hp = "hp",
      `log(gear)` = "gear [log]", `as.factor(cyl)6:I(wt/10)` = "cyl [6] * wt/10",
      `as.factor(cyl)8:I(wt/10)` = "cyl [8] * wt/10", `hp:log(gear)` = "hp * gear [log]"
    )
  )

  m6 <- lm(mpg ~ as.factor(cyl) * log(wt) + hp * log(gear), data = mtcars)
  expect_equal(
    attr(model_parameters(m6), "pretty_names"),
    c(
      `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl [6]",
      `as.factor(cyl)8` = "cyl [8]", `log(wt)` = "wt [log]", hp = "hp",
      `log(gear)` = "gear [log]", `as.factor(cyl)6:log(wt)` = "cyl [6] * wt [log]",
      `as.factor(cyl)8:log(wt)` = "cyl [8] * wt [log]", `hp:log(gear)` = "hp * gear [log]"
    )
  )

  m7 <- lm(mpg ~ as.factor(cyl) * poly(wt, 2) + hp * log(gear), data = mtcars)
  expect_equal(
    attr(model_parameters(m7), "pretty_names"),
    c(
      `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl6",
      `as.factor(cyl)8` = "cyl8", `poly(wt, 2)1` = "wt [1st degree]",
      `poly(wt, 2)2` = "wt [2nd degree]", hp = "hp", `log(gear)` = "gear [log]",
      `as.factor(cyl)6:poly(wt, 2)1` = "cyl6 * wt [1st degree]", `as.factor(cyl)8:poly(wt, 2)1` = "cyl8 * wt [1st degree]",
      `as.factor(cyl)6:poly(wt, 2)2` = "cyl6 * wt [2nd degree]", `as.factor(cyl)8:poly(wt, 2)2` = "cyl8 * wt [2nd degree]",
      `hp:log(gear)` = "hp * gear [log]"
    )
  )

  m8 <- lm(mpg ~ as.factor(cyl) * I(wt^2) + hp * log(gear), data = mtcars)
  expect_equal(
    attr(model_parameters(m8), "pretty_names"),
    c(
      `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl [6]",
      `as.factor(cyl)8` = "cyl [8]", `I(wt^2)` = "wt^2", hp = "hp",
      `log(gear)` = "gear [log]", `as.factor(cyl)6:I(wt^2)` = "cyl [6] * wt^2",
      `as.factor(cyl)8:I(wt^2)` = "cyl [8] * wt^2", `hp:log(gear)` = "hp * gear [log]"
    )
  )
})
