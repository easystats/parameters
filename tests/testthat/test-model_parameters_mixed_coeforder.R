test_that("model_parameters.mixed.coeforder", {
  skip_if_not_installed("lme4")
  set.seed(1)
  dat <- data.frame(
    TST.diff = runif(100, 0, 100),
    Exposition = as.factor(sample(0:2, 100, TRUE)),
    Gruppe = as.factor(sample(0:1, 100, TRUE)),
    Kennung = as.factor(sample(1:5, 100, TRUE))
  )

  m <- lme4::lmer(TST.diff ~ Exposition + Gruppe + Gruppe:Exposition + (1 | Kennung), data = dat)
  cs <- coef(summary(m))
  mp <- model_parameters(m, effects = "fixed")
  expect_equal(mp$Parameter, rownames(cs))
})
