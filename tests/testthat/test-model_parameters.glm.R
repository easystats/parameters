if (require("testthat") && require("parameters")) {
  data(mtcars)
  test_that("model_parameters.lm", {
    model <- lm(mpg ~ wt, data = mtcars)
    params <- model_parameters(model)
    testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))
    testthat::expect_equal(params$CI_high, c(41.119752761418, -4.20263490802709), tolerance = 1e-3)
    testthat::expect_equal(attributes(params)$sigma, 3.045882, tolerance = 1e-3)

    params <- model_parameters(model, ci = c(0.8, 0.9))
    testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 10))

    params <- model_parameters(model, dispersion = TRUE, bootstrap = TRUE, n = 500)
    testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 6))

    model <- lm(mpg ~ wt + cyl, data = mtcars)
    params <- model_parameters(model)
    testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 8))

    model <- lm(mpg ~ wt * cyl, data = mtcars)
    params <- model_parameters(model)
    testthat::expect_equal(c(nrow(params), ncol(params)), c(4, 8))

    params <- model_parameters(model, component = "conditional", effects = "fixed")
  })



  test_that("print digits model_parameters.lm", {
    model <- lm(mpg ~ wt, data = mtcars)
    params <- model_parameters(model, digits = 4, ci_digits = 5)
    out <- capture.output(print(params))
    expect_equal(out[3], "(Intercept) |     37.2851 | 1.8776 | [33.45050, 41.11975] | 19.8576 | < .001")
  })


  test_that("model_parameters.glm - binomial", {
    set.seed(333)
    model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

    params <- model_parameters(model)
    testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 8))

    params <- suppressWarnings(model_parameters(model, bootstrap = TRUE, n = 500))
    testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 5))

    params <- model_parameters(model, component = "conditional", effects = "fixed")
  })
}
