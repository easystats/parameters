if (require("testthat") && require("insight") && require("parameters") && require("cplm")) {
  data("FineRoot")
  model <- cpglmm(RLD ~ Stock + Spacing + (1 | Plant), data = FineRoot)

  test_that("model_parameters.cpglmm", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(0.1308, 0.2514, 0.2, 0.1921), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Effects")
    )
  })
}
