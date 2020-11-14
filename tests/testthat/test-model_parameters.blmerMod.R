if (require("testthat") && require("parameters") && require("blme")) {
  data(sleepstudy)
  set.seed(123)
  model <- blmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy, cov.prior = NULL)

  test_that("model_parameters.blmerMod", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(6.8246, 1.54579), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "t", "df_error", "p")
    )
  })
}
