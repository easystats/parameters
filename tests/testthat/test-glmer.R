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

  params <- model_parameters(model, effects = "fixed")

  test_that("model_parameters.glmer", {
    expect_equal(params$SE, c(0.22758, 0.30329, 0.32351, 0.42445), tolerance = 1e-2)
  })

  test_that("print model_parameters", {
    out <- utils::capture.output(print(params))
    expect_equal(
      out,
      c("# Fixed Effects",
        "",
        "Parameter   | Log-Odds |   SE |         95% CI |     z |      p",
        "---------------------------------------------------------------",
        "(Intercept) |    -1.36 | 0.23 | [-1.81, -0.91] | -5.98 | < .001",
        "period [2]  |    -0.98 | 0.30 | [-1.57, -0.38] | -3.22 | 0.001 ",
        "period [3]  |    -1.11 | 0.32 | [-1.75, -0.48] | -3.43 | < .001",
        "period [4]  |    -1.56 | 0.42 | [-2.39, -0.73] | -3.67 | < .001"
      ))

    mp <- model_parameters(model, effects = "all", exponentiate = TRUE)
    out <- utils::capture.output(print(mp))
    expect_equal(
      out,
      c("# Fixed Effects",
        "",
        "Parameter   | Odds Ratio |   SE |       95% CI |     z |      p",
        "---------------------------------------------------------------",
        "(Intercept) |       0.26 | 0.06 | [0.16, 0.40] | -5.98 | < .001",
        "period [2]  |       0.38 | 0.11 | [0.21, 0.68] | -3.22 | 0.001 ",
        "period [3]  |       0.33 | 0.11 | [0.17, 0.62] | -3.43 | < .001",
        "period [4]  |       0.21 | 0.09 | [0.09, 0.48] | -3.67 | < .001",
        "",
        "# Random Effects",
        "",
        "Parameter            | Coefficient",
        "----------------------------------",
        "SD (Intercept: herd) |        0.64",
        "SD (Residual)        |        1.00"))
  })



  test_that("model_parameters.glmer ml1", {
    params <- model_parameters(model, df_method = "ml1", effects = "fixed")
    expect_equal(params$SE, c(0.26093, 0.31854, 0.34172, 0.45132), tolerance = 1e-2)
    expect_equal(params$df, c(54, 54, 54, 54), tolerance = 1e-2)
  })

  test_that("model_parameters.glmer betwithin", {
    params <- model_parameters(model, df_method = "betwithin", effects = "fixed")
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
    params <- model_parameters(model, effects = "fixed")
    expect_equal(params$SE, c(0.66539, 0.36178, 0.36223, 0.45528, 0.2379), tolerance = 1e-2)
  })

  test_that("model_parameters.glmer ml1", {
    params <- model_parameters(model, df_method = "ml1", effects = "fixed")
    expect_equal(params$SE, c(0.68563, 0.37647, 0.37702, 0.48758, 0.23907), tolerance = 1e-2)
    expect_equal(params$df, c(53, 53, 53, 53, 53), tolerance = 1e-2)
  })

  test_that("model_parameters.glmer betwithin", {
    params <- model_parameters(model, df_method = "betwithin", effects = "fixed")
    expect_equal(params$SE, c(0.66672, 0.36276, 0.36322, 0.4575, 0.24484), tolerance = 1e-2)
    expect_equal(params$df, c(821, 821, 821, 821, 9), tolerance = 1e-2)
  })
}

