if (requiet("testthat") && requiet("insight") && requiet("parameters") && requiet("mfx")) {
  set.seed(12345)
  n <- 1000
  x <- rnorm(n)

  y <- rbeta(n, shape1 = plogis(1 + 0.5 * x), shape2 = (abs(0.2 * x)))
  y <- (y * (n - 1) + 0.5) / n

  data <- data.frame(y, x)
  model <- betamfx(y ~ x | x, data = data)

  params <- suppressWarnings(model_parameters(model))
  test_that("model_parameters.betamfx", {
    expect_equal(params$Parameter, c("x", "(Intercept)", "x", "(Intercept)", "x"))
    expect_equal(params$Coefficient, c(0.02259, 1.35961, 0.13947, 0.07498, 0.12071), tolerance = 1e-2)
    expect_equal(params$Component, c("marginal", "conditional", "conditional", "precision", "precision"))
  })


  model <- betaor(y ~ x | x, data = data)
  params <- suppressWarnings(model_parameters(model))
  test_that("model_parameters.betaor", {
    expect_equal(params$Parameter, c("(Intercept)", "x"))
    expect_equal(params$Coefficient, c(1.35961, 0.13947), tolerance = 1e-2)
    expect_null(params$Component)
  })


  params <- suppressWarnings(model_parameters(model, component = "all"))
  test_that("model_parameters.betaor", {
    expect_equal(params$Parameter, c("(Intercept)", "x", "(Intercept)", "x"))
    expect_equal(params$Coefficient, unname(do.call(rbind, coef(summary(model$fit)))[, 1]), tolerance = 1e-2)
    expect_equal(params$Component, c("conditional", "conditional", "precision", "precision"))
  })


  set.seed(12345)
  n <- 1000
  x <- rnorm(n)
  y <- rnegbin(n, mu = exp(1 + 0.5 * x), theta = 0.5)

  data <- data.frame(y, x)

  model <- poissonmfx(formula = y ~ x, data = data)
  params <- suppressWarnings(model_parameters(model))
  test_that("model_parameters.poissonmfx", {
    expect_equal(params$Parameter, c("x", "(Intercept)", "x"))
    expect_equal(params$Coefficient, c(1.46009, 0.96036, 0.54496), tolerance = 1e-2)
    expect_equal(params$Component, c("marginal", "conditional", "conditional"))
  })


  params <- suppressWarnings(model_parameters(model, component = "cond"))
  test_that("model_parameters.poissonmfx", {
    expect_equal(params$Parameter, c("(Intercept)", "x"))
    expect_equal(params$Coefficient, c(0.96036, 0.54496), tolerance = 1e-2)
    expect_null(params$Component)
  })
}
