if (require("testthat") &&
    require("parameters") &&
    require("lme4")) {
  data("cbpp")
  set.seed(123)
  model <- glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial(),
    nAGQ = 0
  )

  test_that("model_parameters.glmer", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(0.22758, 0.30329, 0.32351, 0.42445), tolerance = 1e-2)
  })

  test_that("model_parameters.glmer ml1", {
    params <- model_parameters(model, df_method = "ml1")
    expect_equal(params$SE, c(0.26093, 0.31854, 0.34172, 0.45132), tolerance = 1e-2)
    expect_equal(params$df, c(54, 54, 54, 54), tolerance = 1e-2)
  })

  test_that("model_parameters.glmer betwithin", {
    params <- model_parameters(model, df_method = "betwithin")
    expect_equal(params$SE, c(0.27486, 0.32572, 0.35021, 0.46373), tolerance = 1e-2)
    expect_equal(params$df, c(36, 36, 36, 36), tolerance = 1e-2)
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
    expect_equal(params$SE, c(0.66539, 0.36178, 0.36223, 0.45528, 0.2379), tolerance = 1e-2)
  })

  test_that("model_parameters.glmer ml1", {
    params <- model_parameters(model, df_method = "ml1")
    expect_equal(params$SE, c(0.68563, 0.37647, 0.37702, 0.48758, 0.23907), tolerance = 1e-2)
    expect_equal(params$df, c(53, 53, 53, 53, 53), tolerance = 1e-2)
  })

  test_that("model_parameters.glmer betwithin", {
    params <- model_parameters(model, df_method = "betwithin")
    expect_equal(params$SE, c(0.69571, 0.38368, 0.38427, 0.50275, 0.24484), tolerance = 1e-2)
    expect_equal(params$df, c(35, 35, 35, 35, 9), tolerance = 1e-2)
  })
}
