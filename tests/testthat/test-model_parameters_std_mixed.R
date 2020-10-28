.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (require("testthat") &&
    require("parameters") &&
    require("effectsize") &&
    require("lme4")) {

    data(iris)
    set.seed(1234)
    iris$grp <- as.factor(sample(1:3, nrow(iris), replace = TRUE))

    # fit example model
    model <- lme4::lmer(
      Sepal.Length ~ Species * Sepal.Width + Petal.Length + (1 | grp),
      data = iris
    )

    test_that("model_parameters, standardize-refit", {
      params <- model_parameters(model, standardize = "refit")
      testthat::expect_equal(c(nrow(params), ncol(params)), c(7, 8))
      testthat::expect_equal(params$Coefficient, c(0.96949, -1.28631, -1.81461, 0.34791, 1.74252, -0.25421, -0.18834), tolerance = 1e-3)
      testthat::expect_equal(params$SE, c(0.2045, 0.2619, 0.34035, 0.05968, 0.13914, 0.09762, 0.0945), tolerance = 1e-3)
      testthat::expect_equal(params$CI_high, c(1.37031, -0.77301, -1.14754, 0.46488, 2.01523, -0.06287, -0.00312), tolerance = 1e-3)
    })

    test_that("model_parameters, standardize-posthoc", {
      params <- model_parameters(model, standardize = "posthoc")
      testthat::expect_equal(c(nrow(params), ncol(params)), c(7, 8))
      testthat::expect_equal(params$Std_Coefficient, c(0, 0.49679, -0.49355, 0.34791, 1.74252, -0.25421, -0.18834), tolerance = 1e-3)
      testthat::expect_equal(params$SE, c(0, 0.66228, 0.70202, 0.05968, 0.13914, 0.09762, 0.0945), tolerance = 1e-3)
      testthat::expect_equal(params$CI_high, c(0, 1.79483, 0.88238, 0.46488, 2.01523, -0.06287, -0.00312), tolerance = 1e-3)
    })

    test_that("model_parameters, standardize-basic", {
      params <- model_parameters(model, standardize = "basic")
      testthat::expect_equal(c(nrow(params), ncol(params)), c(7, 8))
      testthat::expect_equal(params$Std_Coefficient, c(0, 0.23497, -0.23344, 0.34791, 1.74252, -0.77129, -0.61304), tolerance = 1e-3)
      testthat::expect_equal(params$SE, c(0, 0.31325, 0.33204, 0.05968, 0.13914, 0.2962, 0.30761), tolerance = 1e-3)
      testthat::expect_equal(params$CI_high, c(0, 0.84893, 0.41735, 0.46488, 2.01523, -0.19075, -0.01014), tolerance = 1e-3)
    })

    ## TODO enable when clubSandwich back on CRAN

    # if (require("clubSandwich")) {
    #   test_that("model_parameters, standardize-refit robust", {
    #     params <- model_parameters(model, standardize = "refit", robust = TRUE, vcov_estimation = "CR", vcov_type = "CR1", vcov_args = list(cluster = iris$grp))
    #     testthat::expect_equal(c(nrow(params), ncol(params)), c(7, 8))
    #     testthat::expect_equal(params$Coefficient, c(0.96949, -1.28631, -1.81461, 0.34791, 1.74252, -0.25421, -0.18834), tolerance = 1e-3)
    #     testthat::expect_equal(params$SE, c(0.07726, 0.33406, 0.22647, 0.0524, 0.10092, 0.18537, 0.05552), tolerance = 1e-3)
    #     testthat::expect_equal(params$CI_high, c(1.12224, -0.6259, -1.36691, 0.45151, 1.94204, 0.11227, -0.07858), tolerance = 1e-3)
    #   })
    # }
  }
}
