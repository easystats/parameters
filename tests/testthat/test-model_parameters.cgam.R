.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && requiet("testthat") && requiet("cgam")) {
  test_that("model_parameters - cgam", {
    # cgam -----------------------

    data(cubic)

    # model
    m_cgam <- cgam::cgam(formula = y ~ incr.conv(x), data = cubic)

    df_cgam <- parameters::model_parameters(m_cgam)

    expect_equal(
      df_cgam,
      structure(list(
        Parameter = "(Intercept)", Coefficient = 1.187, SE = 0.3054,
        CI = 0.95, CI_low = 0.569520101908619, CI_high = 1.80447989809138,
        t = 3.8868, df_error = 39.5, p = 4e-04),
        row.names = c(NA, -1L),
        sigma = 2.15946395506817,
        residual_df = 39.5,
        pretty_names = c(`(Intercept)` = "(Intercept)"),
        ci = 0.95,
        test_statistic = "t-statistic",
        verbose = TRUE,
        exponentiate = FALSE,
        ordinal_model = FALSE,
        linear_model = TRUE,
        mixed_model = FALSE,
        n_obs = 50L,
        model_class = "cgam",
        bootstrap = FALSE,
        iterations = 1000,
        robust_vcov = FALSE,
        ignore_group = TRUE,
        ran_pars = TRUE,
        show_summary = FALSE,
        weighted_nobs = 50,
        model_formula = "y ~ incr.conv(x)",
        coefficient_name = "Coefficient",
        zi_coefficient_name = "Log-Odds",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        footer_digits = 3,
        class = c("parameters_model", "see_parameters_model", "data.frame"),
        object_name = "m_cgam"),
      tolerance = 0.01
    )
  })

  # cgamm -----------------------

  test_that("model_parameters - cgamm", {
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
    ans <- cgam::cgamm(formula = y ~ s.incr(x) + (1 | group), reml = TRUE)

    df <- as.data.frame(suppressWarnings(parameters::model_parameters(ans)))

    expect_equal(df,
      structure(
        list(
          Parameter = "(Intercept)",
          Coefficient = 5.5174,
          SE = 0.3631,
          CI = 0.95,
          CI_low = 4.80573707721351,
          CI_high = 6.22906292278649,
          t = 15.1954,
          df_error = 890.4,
          p = 0
        ),
        row.names = 1L,
        sigma = numeric(0),
        residual_df = Inf,
        ci = 0.95,
        test_statistic = "t-statistic",
        verbose = TRUE,
        exponentiate = FALSE,
        ordinal_model = FALSE,
        linear_model = TRUE,
        mixed_model = TRUE,
        model_class = c(
          "cgamm",
          "cgam"
        ),
        bootstrap = FALSE,
        iterations = 1000,
        robust_vcov = FALSE,
        ignore_group = TRUE,
        ran_pars = TRUE,
        show_summary = FALSE,
        model_formula = "y ~ s.incr(x)",
        coefficient_name = "Coefficient",
        zi_coefficient_name = "Log-Odds",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        footer_digits = 3,
        class = "data.frame",
        object_name = "ans"
      ),
      tolerance = 0.001
    )
  })
}
