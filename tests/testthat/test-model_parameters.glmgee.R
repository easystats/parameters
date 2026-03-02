skip_on_cran()
skip_if_not_installed("glmtoolbox")
skip_if_not_installed("withr")

withr::with_options(
  list(parameters_exponentiate = FALSE),
  test_that("model_parameters.glmgee", {
    data(spruces, package = "glmtoolbox")
    fit1 <- glmtoolbox::glmgee(
      size ~ poly(days, 4) + treat,
      id = tree,
      family = Gamma("log"),
      corstr = "AR-M-dependent(1)",
      data = spruces
    )
    out <- model_parameters(fit1)
    expect_snapshot(print(out))
  })
)
