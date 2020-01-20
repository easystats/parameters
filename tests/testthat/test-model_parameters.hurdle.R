if (require("testthat") && require("pscl") && require("parameters")) {
  set.seed(123)
  data("bioChemists", package = "pscl")
  model <- hurdle(formula = art ~ ., data = bioChemists, zero = "geometric")

  test_that("model_parameters.hurdle", {
    params <- model_parameters(model)
    testthat::expect_equal(params$SE, c(0.12246, 0.06522, 0.07283, 0.04845, 0.0313, 0.00228, 0.29552, 0.15911, 0.18082, 0.11113, 0.07956, 0.01302), tolerance = 1e-3)
    testthat::expect_equal(params$Coefficient, unname(coef(model)), tolerance = 1e-3)
    testthat::expect_equal(params$z, unname(c(coef(summary(model))[[1]][, 3], coef(summary(model))[[2]][, 3])), tolerance = 1e-3)
  })
}
