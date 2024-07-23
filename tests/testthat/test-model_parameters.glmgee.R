skip_on_cran()
skip_if_not_installed("glmtoolbox")

test_that("model_parameters.glmgee", {
  data(spruces, package = "glmtoolbox")
  fit1 <- glmtoolbox::glmgee(
    size ~ poly(days,4) + treat,
    id = tree,
    family = Gamma("log"),
    corstr = "AR-M-dependent(1)",
    data = spruces
  )
  out <- model_parameters(fit1)
  expect_snapshot(out, variant = "windows")
})
