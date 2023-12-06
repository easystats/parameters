skip_on_cran()


test_that("model_parameters - cgam", {
  skip_if_not(getRversion() >= "4.0.0")
  skip_if_not_installed("cgam")

  # cgam -----------------------

  data(cubic, package = "cgam")

  # model
  m_cgam <- cgam::cgam(formula = y ~ cgam::incr.conv(x), data = cubic)
  df_cgam <- model_parameters(m_cgam)

  expect_equal(
    df_cgam,
    data.frame(
      Parameter = "(Intercept)",
      Coefficient = 1.187,
      SE = 0.3054,
      CI = 0.95,
      CI_low = 0.569520101908619,
      CI_high = 1.80447989809138,
      t = 3.8868,
      df_error = 39.5,
      p = 4e-04,
      stringsAsFactors = FALSE
    ),
    tolerance = 0.01,
    ignore_attr = TRUE
  )
})

# cgamm -----------------------

test_that("model_parameters - cgamm", {
  skip_if_not(getRversion() >= "4.0.0")
  skip_if_not_installed("cgam")

  # setup
  set.seed(123)

  # simulate a balanced data set with 30 clusters
  # each cluster has 30 data points
  n <- 30
  m <- 30

  # the standard deviation of between cluster error terms is 1
  # the standard deviation of within cluster error terms is 2
  sige <- 1
  siga <- 2

  # generate a continuous predictor
  x <- 1:(m * n)
  for (i in 1:m) {
    x[(n * (i - 1) + 1):(n * i)] <- round(runif(n), 3)
  }
  # generate a group factor
  group <- trunc(0:((m * n) - 1) / n) + 1

  # generate the fixed-effect term
  mu <- 10 * exp(10 * x - 5) / (1 + exp(10 * x - 5))

  # generate the random-intercept term asscosiated with each group
  avals <- rnorm(m, 0, siga)

  # generate the response
  y <- 1:(m * n)
  for (i in 1:m) {
    y[group == i] <- mu[group == i] + avals[i] + rnorm(n, 0, sige)
  }

  # use REML method to fit the model
  ans <- cgam::cgamm(formula = y ~ cgam::s.incr(x) + (1 | group), reml = TRUE)
  df <- suppressWarnings(model_parameters(ans))

  expect_equal(
    df,
    data.frame(
      Parameter = c("(Intercept)", "cgam::s.incr(x)"),
      Coefficient = c(5.5174, NA),
      SE = c(0.3631, NA),
      CI = c(0.95, NA),
      CI_low = c(4.80476838465533, NA),
      CI_high = c(6.23003161534467, NA),
      `t / F` = c(15.1954, NA),
      df = c(NA, 8.4),
      df_error = c(890.4, NA),
      p = c(0, 0),
      Component = c("conditional", "smooth_terms"),
      stringsAsFactors = FALSE
    ),
    tolerance = 0.01,
    ignore_attr = TRUE
  )
})
