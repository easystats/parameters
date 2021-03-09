if (require("testthat") && require("insight") && require("parameters") && require("nlme") && require("lme4")) {
  data("sleepstudy")
  model <- lme(Reaction ~ Days,
    random = ~ 1 + Days | Subject,
    data = sleepstudy
  )

  test_that("model_parameters.lme", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(6.8245, 1.5458), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Effects")
    )
  })
}
