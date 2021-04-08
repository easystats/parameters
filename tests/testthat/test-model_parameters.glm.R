if (require("testthat") && require("parameters") && require("boot")) {
  data(mtcars)
  test_that("model_parameters.lm", {
    model <- lm(mpg ~ wt, data = mtcars)
    params <- model_parameters(model)
    expect_equal(c(nrow(params), ncol(params)), c(2, 9))
    expect_equal(params$CI_high, c(41.119752761418, -4.20263490802709), tolerance = 1e-3)
    expect_equal(attributes(params)$sigma, 3.045882, tolerance = 1e-3)

    params <- model_parameters(model, ci = c(0.8, 0.9))
    expect_equal(c(nrow(params), ncol(params)), c(2, 10))

    params <- model_parameters(model, dispersion = TRUE, bootstrap = TRUE, iterations = 500)
    expect_equal(c(nrow(params), ncol(params)), c(2, 7))

    model <- lm(mpg ~ wt + cyl, data = mtcars)
    params <- model_parameters(model)
    expect_equal(c(nrow(params), ncol(params)), c(3, 9))

    model <- lm(mpg ~ wt * cyl, data = mtcars)
    params <- model_parameters(model)
    expect_equal(c(nrow(params), ncol(params)), c(4, 9))

    params <- model_parameters(model, component = "conditional", effects = "fixed")
  })



  test_that("print digits model_parameters.lm", {
    model <- lm(mpg ~ wt, data = mtcars)
    params <- model_parameters(model, digits = 4, ci_digits = 5)
    out <- capture.output(print(params))
    expect_equal(out[3], "(Intercept) |     37.2851 | 1.8776 | [33.45050, 41.11975] | 19.8576 | < .001")
  })


  test_that("print digits model_parameters.lm", {
    model <- lm(mpg ~ wt, data = mtcars)
    params <- model_parameters(model, summary = TRUE)
    out <- capture.output(print(params))
    expect_equal(
      out,
      c("Parameter   | Coefficient |   SE |         95% CI | t(30) |      p",
        "------------------------------------------------------------------",
        "(Intercept) |       37.29 | 1.88 | [33.45, 41.12] | 19.86 | < .001",
        "wt          |       -5.34 | 0.56 | [-6.49, -4.20] | -9.56 | < .001",
        "",
        "Model: mpg ~ wt (32 Observations)",
        "Residual standard deviation: 3.046 (df = 30)",
        "R2: 0.753; adjusted R2: 0.745"
    ))
  })


  test_that("model_parameters.glm - binomial", {
    set.seed(333)
    model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

    params <- model_parameters(model)
    expect_equal(c(nrow(params), ncol(params)), c(3, 9))

    params <- suppressWarnings(model_parameters(model, bootstrap = TRUE, iterations = 500))
    expect_equal(c(nrow(params), ncol(params)), c(3, 6))

    params <- model_parameters(model, component = "conditional", effects = "fixed")
  })
}
