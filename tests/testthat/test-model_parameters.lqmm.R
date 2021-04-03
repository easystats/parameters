.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("testthat") && require("lqmm") && require("parameters")) {
  # lqm -----------------------

  test_that("model_parameters - lqm", {
    # data
    set.seed(123)
    n <- 500
    p <- 1:3 / 4
    set.seed(123)
    x <- runif(n, 0, 1)
    y <- 30 + x + rnorm(n)
    test <<- data.frame(x, y)

    # model
    set.seed(123)
    fit.lqm <-
      lqmm::lqm(
        y ~ x,
        data = test,
        tau = p,
        control = list(verbose = FALSE, loop_tol_ll = 1e-9),
        fit = TRUE
      )

    df_lqm <- as.data.frame(model_parameters(fit.lqm))

    expect_equal(df_lqm$Coefficient,
      c(
        29.3220715172958, 1.1244506550584, 29.9547605920406, 1.1822574944936,
        30.6283792821576, 1.25165747424685
      ),
      tolerance = 0.001
    )
  })


  # lqmm -----------------------

  test_that("model_parameters - lqmm", {

    # setup
    set.seed(123)

    # data
    M <- 50
    n <- 10
    set.seed(123)
    x <- runif(n * M, 0, 1)
    group <- rep(1:M, each = n)
    y <- 10 * x + rep(rnorm(M, 0, 2), each = n) + rchisq(n * M, 3)
    test <<- data.frame(x, y, group)

    # model
    set.seed(123)
    fit.lqmm <-
      lqmm::lqmm(
        fixed = y ~ x,
        random = ~1,
        group = group,
        data = test,
        tau = 0.5,
        nK = 11,
        type = "normal"
      )

    df_lqmm <- as.data.frame(model_parameters(fit.lqmm))

    expect_equal(df_lqmm,
                 structure(
                   list(
                     Parameter = c("(Intercept)", "x"),
                     Coefficient = c(3.44347538706013,
                                     9.25833091219961),
                     SE = c(0.491049614414579, 0.458163772053399),
                     CI = c(0.95, 0.95),
                     CI_low = c(2.47868633791118, 8.35815427623814),
                     CI_high = c(4.40826443620908, 10.1585075481611),
                     t = c(7.01247956617455,
                           20.207470509302),
                     df_error = c(497L, 497L),
                     p = c(6.34497395571023e-09,
                           2.05172540270515e-25)
                   ),
                   row.names = 1:2,
                   pretty_names = c(`(Intercept)` = "(Intercept)",
                                    x = "x"),
                   ci = 0.95,
                   verbose = TRUE,
                   exponentiate = FALSE,
                   ordinal_model = FALSE,
                   linear_model = TRUE,
                   mixed_model = TRUE,
                   n_obs = 500L,
                   model_class = "lqmm",
                   bootstrap = FALSE,
                   iterations = 1000,
                   ignore_group = TRUE,
                   ran_pars = TRUE,
                   weighted_nobs = 500,
                   model_formula = "y ~ x",
                   coefficient_name = "Coefficient",
                   zi_coefficient_name = "Log-Odds",
                   digits = 2,
                   ci_digits = 2,
                   p_digits = 3,
                   class = "data.frame",
                   object_name = "fit.lqmm"
                 ),
                 tolerance = 0.001
    )
  })
}
