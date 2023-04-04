if (requiet("glmmTMB") && requiet("TMB") && requiet("lme4")) {
  test_that("glmmTMB profiled and uniroot CI work", {
    skip_on_cran()
    data(sleepstudy, package = "lme4")
    m <- glmmTMB(Reaction ~ Days + (Days | Subject), data = sleepstudy)
    expect_silent({
      mp1 <- model_parameters(m, ci_method = "uniroot")
    })
    expect_silent({
      mp2 <- model_parameters(m, ci_method = "profile")
    })
    expect_snapshot(print(m1))
    expect_snapshot(print(m2))
  })
}
