test_that("glmmTMB profiled and uniroot CI work", {
  skip_on_cran()
  skip_if_not_installed("TMB")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")

  m <- glmmTMB::glmmTMB(Reaction ~ Days + (Days | Subject), data = sleepstudy)
  expect_silent({
    mp1 <- model_parameters(m, ci_method = "uniroot")
  })
  expect_silent({
    mp2 <- model_parameters(m, ci_method = "profile")
  })
  expect_snapshot(print(mp1))
  expect_snapshot(print(mp2))
})
