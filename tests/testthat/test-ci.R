test_that("ci", {
  skip_if_not_installed("lme4")

  model <- lm(mpg ~ wt, data = mtcars)
  expect_equal(suppressMessages(ci(model))[1, 3], 33.4505, tolerance = 0.01)
  expect_equal(suppressMessages(ci(model, ci = c(0.7, 0.8)))[1, 3], 35.30486, tolerance = 0.01)

  model <- glm(vs ~ wt, family = "binomial", data = mtcars)
  expect_equal(suppressMessages(ci(model))[1, 3], 1.934013, tolerance = 0.01)

  model <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  expect_equal(suppressMessages(ci(model, method = "normal"))[1, 3], -0.335063, tolerance = 0.01)

  model <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  expect_equal(ci(model)[1, 3], -0.3795646, tolerance = 0.01)

  set.seed(1)
  val <- ci(model, method = "boot")[1, 3]
  expect_equal(val, -0.555424, tolerance = 0.01)

  model <- lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")
  expect_equal(ci(model)[1, 3], -0.7876679, tolerance = 0.01)

  model <- lme4::glmer(vs ~ drat + cyl + (1 | gear), data = mtcars, family = "binomial")
  expect_equal(ci(model)[1, 3], -48.14195, tolerance = 0.01)
})


test_that("vs. sandwich & lmtest", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  model <- lm(mpg ~ wt, data = mtcars)
  known <- lmtest::coefci(model, vcov = sandwich::vcovHC)
  unknown <- ci(model, vcov = sandwich::vcovHC)
  expect_equal(unknown[["CI_low"]], known[, "2.5 %"], ignore_attr = TRUE)
  expect_equal(unknown[["CI_high"]], known[, "97.5 %"], ignore_attr = TRUE)

  model <- glm(am ~ wt, data = mtcars, family = binomial)
  known <- lmtest::coefci(model, vcov = sandwich::vcovHC)
  unknown <- ci(model, vcov = sandwich::vcovHC, method = "wald")
  expect_equal(unknown[["CI_low"]], known[, "2.5 %"], ignore_attr = TRUE)
  expect_equal(unknown[["CI_high"]], known[, "97.5 %"], ignore_attr = TRUE)

  suppressMessages(
    expect_message(ci(model, vcov = sandwich::vcovHC), regexp = "vcov.*are not available with.*profile")
  )
})
