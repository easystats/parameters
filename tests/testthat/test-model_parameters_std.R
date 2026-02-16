skip_on_cran()

skip_if_not_installed("effectsize")
data(mtcars)
mtcars$am <- as.factor(mtcars$am)
d <- mtcars
model <- lm(mpg ~ wt * am, data = d)

test_that("model_parameters, standardize-refit", {
  params <- model_parameters(model, standardize = "refit")
  expect_shape(params, dim = c(4L, 9L))
  expect_equal(
    params$Coefficient,
    c(-0.14183, -0.61463, -0.35967, -0.86017),
    tolerance = 1e-3
  )
  expect_equal(params$SE, c(0.12207, 0.12755, 0.23542, 0.23454), tolerance = 1e-3)
  expect_equal(params$CI_high, c(0.10821, -0.35336, 0.12257, -0.37973), tolerance = 1e-3)
})

test_that("model_parameters, standardize-posthoc", {
  params <- model_parameters(model, standardize = "posthoc")
  expect_shape(params, dim = c(4L, 9L))
  expect_equal(
    params$Std_Coefficient,
    c(0, -0.61463, 2.46865, -0.87911),
    tolerance = 1e-3
  )
  expect_equal(params$SE, c(NA, 0.12755, 0.7075, 0.23971), tolerance = 1e-3)
  expect_equal(params$CI_high, c(NA, -0.35336, 3.91789, -0.38809), tolerance = 0.1)
})

test_that("model_parameters, standardize-basic", {
  params <- model_parameters(model, standardize = "basic")
  expect_shape(params, dim = c(4L, 9L))
  expect_equal(
    params$Std_Coefficient,
    c(0, -0.61463, 1.23183, -1.11016),
    tolerance = 1e-3
  )
  expect_equal(params$SE, c(NA, 0.12755, 0.35303, 0.30271), tolerance = 1e-3)
  expect_equal(params$CI_high, c(NA, -0.35336, 1.95499, -0.4901), tolerance = 0.1)
})

test_that("model_parameters, standardize-smart", {
  params <- model_parameters(model, standardize = "smart")
  expect_shape(params, dim = c(4L, 9L))
  expect_equal(
    params$Std_Coefficient,
    c(0, -0.61463, 2.41278, -0.85922),
    tolerance = 1e-3
  )
  expect_equal(params$SE, c(NA, 0.12755, 0.69148, 0.23428), tolerance = 1e-3)
  expect_equal(params$CI_high, c(NA, -0.35336, 3.82922, -0.37931), tolerance = 0.1)
})

test_that("model_parameters, glm", {
  mod <- glm(vs ~ wt + am, data = d, family = binomial)
  params <- model_parameters(mod, standardize = "basic")
  expect_shape(params, dim = c(3L, 9L))
  expect_equal(params$Std_Coefficient, c(NA, -6.193337, -3.060555), tolerance = 1e-3)
  expect_equal(params$SE, c(NA, 2.372399, 1.347360), tolerance = 1e-3)
  expect_equal(params$CI_high, c(NA, -2.5714096, -0.9275898), tolerance = 0.1)
})
