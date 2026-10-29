skip_on_cran()
skip_if_not_installed("merTools")
skip_if_not_installed("lme4")

test_that("model_parameters.merModList", {
  data(sleepstudy, package = "lme4")
  set.seed(123)
  sim_list <- replicate(
    n = 3,
    expr = sleepstudy[sample(row.names(sleepstudy), 180), ],
    simplify = FALSE
  )
  mod <- suppressWarnings(merTools::lmerModList(
    "Reaction ~ Days + (Days | Subject)",
    data = sim_list
  ))
  out <- suppressWarnings(model_parameters(mod))
  expect_identical(out$Parameter, c("(Intercept)", "Days"))

  ci_out <- ci(mod, component = "conditional")
  expect_identical(ci_out$Parameter, c("(Intercept)", "Days"))

  # `dof` is passed on once, not twice
  ci_dof <- ci(mod, dof = 10)
  ci_inf <- ci(mod, dof = Inf)
  expect_true(all(ci_dof$CI_high - ci_dof$CI_low > ci_inf$CI_high - ci_inf$CI_low))
})
