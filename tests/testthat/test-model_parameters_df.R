.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && requiet("testthat") && requiet("parameters")) {


  # PROreg ---------------------------

  if (requiet("PROreg")) {
    set.seed(1234)

    # defining the parameters
    k <- 100
    m <- 10
    phi <- 0.5
    beta <- c(1.5, -1.1)
    sigma <- 0.5

    # simulating the covariate and random effects
    x <- runif(k, 0, 10)
    X <- model.matrix(~x)
    z <- as.factor(rBI(k, 4, 0.5, 2))
    Z <- model.matrix(~ z - 1)
    u <- rnorm(5, 0, sigma)

    # the linear predictor and simulated response variable
    eta <- beta[1] + beta[2] * x + crossprod(t(Z), u)
    p <- 1 / (1 + exp(-eta))
    y <- rBB(k, m, p, phi)
    dat <- data.frame(cbind(y, x, z))
    dat$z <- as.factor(dat$z)

    # apply the model
    model <- PROreg::BBmm(
      fixed.formula = y ~ x,
      random.formula = ~z,
      m = m,
      data = dat
    )

    test_that("model_parameters.BBmm", {
      params <- suppressWarnings(model_parameters(model))
      expect_equal(params$df_error, c(96, 96), tolerance = 1e-3)
      expect_equal(params$CI_low, c(0.26363, -1.46645), tolerance = 1e-3)
      expect_equal(params$p, c(0.00811, 0), tolerance = 1e-3)

      params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
      expect_equal(params$df_error, c(Inf, Inf), tolerance = 1e-3)
      expect_equal(params$CI_low, c(0.27359, -1.46136), tolerance = 1e-3)
      expect_equal(params$p, c(0.00811, 0), tolerance = 1e-3)
    })


    set.seed(18)
    # we simulate a covariate, fix the paramters of the beta-binomial
    # distribution and simulate a response variable.
    # then we apply the model, and try to get the same values.
    k <- 1000
    m <- 10
    x <- rnorm(k, 5, 3)
    beta <- c(-10, 2)
    p <- 1 / (1 + exp(-1 * (beta[1] + beta[2] * x)))
    phi <- 1.2
    y <- PROreg::rBB(k, m, p, phi)

    # model
    model <- PROreg::BBreg(y ~ x, m)

    test_that("model_parameters.BBreg", {
      params <- suppressWarnings(model_parameters(model))
      expect_equal(params$df_error, c(997, 997), tolerance = 1e-3)
      expect_equal(params$CI_low, c(-11.08184, 1.84727), tolerance = 1e-3)
      expect_equal(params$p, c(0, 0), tolerance = 1e-3)

      params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
      expect_equal(params$df_error, c(Inf, Inf), tolerance = 1e-3)
      expect_equal(params$CI_low, c(-11.08069, 1.84749), tolerance = 1e-3)
      expect_equal(params$p, c(0, 0), tolerance = 1e-3)
    })

  }



  # MASS / nnet ---------------------------

  if (requiet("MASS") && requiet("nnet")) {
    set.seed(123)
    utils::example(topic = birthwt, echo = FALSE)

    # model
    model <- nnet::multinom(
      formula = low ~ .,
      data = bwt,
      trace = FALSE
    )

    test_that("model_parameters.multinom", {
      params <- suppressWarnings(model_parameters(model))
      expect_equal(params$df_error, c(178, 178, 178, 178, 178, 178, 178, 178, 178, 178, 178), tolerance = 1e-3)
      expect_equal(params$CI_low, c(-1.6332, -0.11362, -0.02963, 0.13471, -0.17058,
                                    -0.08325, 0.39528, 0.49086, -0.23614, -1.38245, -0.72163), tolerance = 1e-3)
      expect_equal(params$p, c(0.50926, 0.33729, 0.02833, 0.02736, 0.11049, 0.07719, 0.00575,
                               0.00866, 0.14473, 0.36392, 0.69537), tolerance = 1e-3)

      params <- suppressWarnings(model_parameters(model, ci_method = "normal"))
      expect_equal(params$df_error, c(Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf, Inf), tolerance = 1e-3)
      expect_equal(params$CI_low, c(-1.6165, -0.1131, -0.02953, 0.1419, -0.16439, -0.07755, 0.40173,
                                    0.50053, -0.22991, -1.37601, -0.71551), tolerance = 1e-3)
      expect_equal(params$p, c(0.5084, 0.33599, 0.02706, 0.0261, 0.10872, 0.07548, 0.00518,
                               0.00794, 0.14296, 0.36269, 0.6949), tolerance = 1e-3)
    })
  }
}

