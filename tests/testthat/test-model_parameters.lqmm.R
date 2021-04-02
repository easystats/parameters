if (require("testthat") && require("lqmm")) {
  # lqm -----------------------

  test_that("model_parameters - lqm", {
    # data
    n <- 500
    p <- 1:3 / 4
    set.seed(123)
    test <- data.frame(x = runif(n, 0, 1))
    test$y <- 30 + test$x + rnorm(n)

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
}
