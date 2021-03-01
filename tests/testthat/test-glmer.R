.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  require("testthat") &&
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
    expect_equal(params$SE, c(0.23009, 0.30433, 0.32476, 0.42632), tolerance = 1e-2)
    expect_equal(params$df, c(822, 822, 822, 822), tolerance = 1e-2)
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
    expect_equal(params$SE, c(0.66672, 0.36276, 0.36322, 0.4575, 0.24484), tolerance = 1e-2)
    expect_equal(params$df, c(821, 821, 821, 821, 9), tolerance = 1e-2)
  })
}
