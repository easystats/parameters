if (requiet("testthat") && requiet("insight") && requiet("parameters") && requiet("nlme") && requiet("lme4")) {
  data("sleepstudy")
  model <- lme(Reaction ~ Days,
    random = ~ 1 + Days | Subject,
    data = sleepstudy
  )

  test_that("model_parameters.lme", {
    params <- model_parameters(model, effects = "fixed")
    expect_equal(params$SE, c(6.8245, 1.5458), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Effects")
    )
  })

  test_that("model_parameters.lme", {
    params <- model_parameters(model, effects = "all")
    expect_equal(params$Coefficient, c(251.4051, 10.46729, 24.74024, 5.9221, 0.066, 25.59184), tolerance = 1e-3)
    expect_equal(params$SE, c(6.82452, 1.54578, NA, NA, NA, NA), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c(
        "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t",
        "df_error", "p", "Effects", "Group"
      )
    )
  })
}
