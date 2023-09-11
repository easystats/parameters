skip_if_not_installed("boot")

test_that("model_parameters.lm", {
  model <- lm(mpg ~ wt, data = mtcars)
  params <- model_parameters(model, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(2L, 9L))
  expect_equal(params$CI_high, c(41.119752761418, -4.20263490802709), tolerance = 1e-3)
  expect_equal(attributes(params)$sigma, 3.045882, tolerance = 1e-3)

  params <- model_parameters(model, ci = c(0.8, 0.9), verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(2L, 10L))

  params <- model_parameters(model, dispersion = TRUE, bootstrap = TRUE, iterations = 500, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(2L, 7L))

  model <- lm(mpg ~ wt + cyl, data = mtcars)
  params <- model_parameters(model, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(3L, 9L))

  model <- lm(mpg ~ wt * cyl, data = mtcars)
  params <- model_parameters(model, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(4L, 9L))

  params <- model_parameters(model, component = "conditional", effects = "fixed", verbose = FALSE)
})

test_that("print digits model_parameters.lm", {
  model <- lm(mpg ~ wt, data = mtcars)
  params <- model_parameters(model, digits = 4, ci_digits = 5, verbose = FALSE)
  out <- capture.output(print(params))
  expect_identical(out[3], "(Intercept) |     37.2851 | 1.8776 | [33.45050, 41.11975] | 19.8576 | < .001")
})


test_that("print digits model_parameters.lm", {
  skip_if_not_installed("performance")
  model <- lm(mpg ~ wt, data = mtcars)

  params <- model_parameters(model, summary = TRUE, verbose = FALSE)
  expect_snapshot(params)

  params <- model_parameters(model, summary = FALSE, verbose = FALSE)
  expect_snapshot(params)
})

test_that("model_parameters.glm - binomial", {
  set.seed(333)
  model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

  params <- model_parameters(model, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(3L, 9L))

  params <- suppressWarnings(model_parameters(model, bootstrap = TRUE, iterations = 500, verbose = FALSE))
  expect_identical(c(nrow(params), ncol(params)), c(3L, 6L))

  params <- model_parameters(model, component = "conditional", effects = "fixed", verbose = FALSE)
})

test_that("model_parameters.glm - Gamma - print", {
  # test printing for prevalence ratios
  clotting <- data.frame(
    u = c(5, 10, 15, 20, 30, 40, 60, 80, 100),
    lot1 = c(118, 58, 42, 35, 27, 25, 21, 19, 18),
    lot2 = c(69, 35, 26, 21, 18, 16, 13, 12, 12)
  )
  m <- glm(lot1 ~ log(u), data = clotting, family = Gamma("log"))
  mp <- model_parameters(m, exponentiate = TRUE)
  expect_snapshot(mp)
})

test_that("model_parameters.glm - glm, identity link", {
  data(mtcars)
  m <- glm(am ~ vs, data = mtcars, family = binomial(link = "identity"))
  p <- model_parameters(m)
  expect_identical(attributes(p)$coefficient_name, "Risk")
})
