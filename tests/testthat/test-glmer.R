if (require("testthat") &&
    require("parameters") &&
    require("lme4")) {
  data("cbpp")
  model <- glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial(),
    nAGQ = 0
  )

  test_that("model_parameters.glmer", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(0.22758, 0.30329, 0.32351, 0.42445), tolerance = 1e-3)
    expect_equal(params$df, c(51, 51, 51, 51), tolerance = 1e-3)
  })

  test_that("model_parameters.glmer ml1", {
    params <- model_parameters(model, df_method = "ml1")
    expect_equal(params$SE, c(0.22567, 0.30239, 0.32245, 0.42288), tolerance = 1e-3)
    expect_equal(params$df, c(54, 54, 54, 54), tolerance = 1e-3)
  })

  test_that("model_parameters.glmer betwithin", {
    params <- model_parameters(model, df_method = "betwithin")
    expect_equal(params$SE, c(0.24116, 0.30985, 0.33131, 0.43592), tolerance = 1e-3)
    expect_equal(params$df, c(36, 36, 36, 36), tolerance = 1e-3)
  })

  set.seed(123)
  cbpp$time <- runif(nrow(cbpp), 1, 4)
  model <- glmer(
    cbind(incidence, size - incidence) ~ period + time + (1 + time | herd),
    data = cbpp,
    family = binomial(),
    nAGQ = 0
  )

  test_that("model_parameters.glmer", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(0.66539, 0.36178, 0.36223, 0.45528, 0.2379), tolerance = 1e-3)
    expect_equal(params$df, c(48, 48, 48, 48, 48), tolerance = 1e-3)
  })

  test_that("model_parameters.glmer ml1", {
    params <- model_parameters(model, df_method = "ml1")
    expect_equal(params$SE, c(0.66328, 0.36025, 0.3607, 0.45195, 0.23778), tolerance = 1e-3)
    expect_equal(params$df, c(53, 53, 53, 53, 53), tolerance = 1e-3)
  })

  test_that("model_parameters.glmer betwithin", {
    params <- model_parameters(model, df_method = "betwithin")
    expect_equal(params$SE, c(0.67359, 0.36769, 0.36818, 0.46802, 0.24352), tolerance = 1e-3)
    expect_equal(params$df, c(35, 35, 35, 35, 9), tolerance = 1e-3)
  })
}
