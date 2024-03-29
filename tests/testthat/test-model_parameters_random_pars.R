skip_on_cran()

skip_if_not_installed("lme4")
data(sleepstudy, package = "lme4")

model <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
mp <- model_parameters(model, effects = "random")

test_that("model_parameters-random pars 1", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(6.81191, 1.72707), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(25.90983, 27.78454), tolerance = 1e-3)
  expect_equal(mp$Parameter, c("SD (Intercept)", "SD (Observations)"))
})

model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
mp <- model_parameters(model, effects = "random")

test_that("model_parameters-random pars 2", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(5.83626, 1.24804, 0.31859, 1.50801), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(15.5817, 3.91828, -0.50907, 22.80044), tolerance = 1e-3)
  expect_equal(mp$Parameter, c("SD (Intercept)", "SD (Days)", "Cor (Intercept~Days)", "SD (Observations)"))
})

model <- lme4::lmer(Reaction ~ Days + (1 + Days || Subject), data = sleepstudy)
mp <- model_parameters(model, effects = "random")

test_that("model_parameters-random pars 3", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(5.66046, 1.21291, 1.50063), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(16.08784, 4.0261, 22.78698), tolerance = 1e-3)
  expect_equal(mp$Parameter, c("SD (Intercept)", "SD (Days)", "SD (Observations)"))
})

model <- lme4::lmer(Reaction ~ Days + (0 + Days || Subject), data = sleepstudy)
mp <- model_parameters(model, effects = "random")

test_that("model_parameters-random pars 4", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(1.31507, 1.6171), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(5.09041, 26.01525), tolerance = 1e-3)
  expect_equal(mp$Parameter, c("SD (Days)", "SD (Observations)"))
})

data(sleepstudy, package = "lme4")
set.seed(12345)
sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$subgrp <- NA
for (i in 1:5) {
  filter_group <- sleepstudy$grp == i
  sleepstudy$subgrp[filter_group] <-
    sample(1:30, size = sum(filter_group), replace = TRUE)
}

model <- lme4::lmer(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = sleepstudy)
mp <- model_parameters(model, effects = "random", ci_random = TRUE)

test_that("model_parameters-random pars 5", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(8.92501, 6.80902, 6.70278, 2.41892), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(0.37493, 25.90517, 0.00135, 25.92818), tolerance = 1e-3)
  expect_equal(mp$Parameter, c("SD (Intercept)", "SD (Intercept)", "SD (Intercept)", "SD (Observations)"))
})

model <- lme4::lmer(Reaction ~ Days + (1 | grp / subgrp), data = sleepstudy)
mp <- model_parameters(model, effects = "random")

test_that("model_parameters-random pars 6", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(11.37581, 10.02558, 3.45893), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(1.33029, 0.00166, 40.13353), tolerance = 1e-3)
  expect_equal(mp$Parameter, c("SD (Intercept)", "SD (Intercept)", "SD (Observations)"))
})

data(sleepstudy, package = "lme4")
sleepstudy$Days2 <- cut(sleepstudy$Days, breaks = c(-1, 3, 6, 10))

model <- lme4::lmer(Reaction ~ Days2 + (1 + Days2 | Subject), data = sleepstudy)
mp <- model_parameters(model, effects = "random")

test_that("model_parameters-random pars 7", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(5.68189, 5.16887, 8.47536, 0.3384, 0.47038, 0.41966, 1.7238), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(16.7131, 21.12065, 24.1964, -0.36662, -0.59868, -0.93174, 24.18608), tolerance = 1e-3)
  expect_equal(
    mp$Parameter,
    c(
      "SD (Intercept)", "SD (Days2(3,6])", "SD (Days2(6,10])", "Cor (Intercept~Days2(3,6])",
      "Cor (Intercept~Days2(6,10])", "Cor (Days2(3,6]~Days2(6,10])",
      "SD (Observations)"
    )
  )
})

model <- lme4::lmer(Reaction ~ Days2 + (0 + Days2 | Subject), data = sleepstudy)
mp <- model_parameters(model, effects = "random")

test_that("model_parameters-random pars 8", {
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(5.68188, 4.951, 9.773, 0.34887, 0.59977, 0.3494, 1.7238), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(16.713, 37.06178, 36.14261, -0.65336, -0.92243, -0.99569, 24.18612), tolerance = 1e-3)
  expect_equal(
    mp$Parameter,
    c(
      "SD (Days2(-1,3])", "SD (Days2(3,6])", "SD (Days2(6,10])",
      "Cor (Days2(-1,3]~Days2(3,6])", "Cor (Days2(-1,3]~Days2(6,10])",
      "Cor (Days2(3,6]~Days2(6,10])", "SD (Observations)"
    )
  )
})

test_that("model_parameters-random pars 9", {
  suppressMessages(
    model <- lme4::lmer(Reaction ~ Days2 + (1 + Days2 || Subject), data = sleepstudy)
  )
  mp <- model_parameters(model, effects = "random", verbose = FALSE)

  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_true(all(is.na(mp$SE)))
  expect_equal(
    mp$Parameter,
    c(
      "SD (Intercept)", "SD (Days2(-1,3])", "SD (Days2(3,6])", "SD (Days2(6,10])",
      "Cor (Days2(-1,3]~Days2(3,6])", "Cor (Days2(-1,3]~Days2(6,10])",
      "Cor (Days2(3,6]~Days2(6,10])", "SD (Observations)"
    )
  )
})

test_that("model_parameters-random pars 10", {
  model <- lme4::lmer(Reaction ~ Days2 + (0 + Days2 || Subject), data = sleepstudy)
  mp <- model_parameters(model, effects = "random")
  expect_equal(mp$Coefficient, as.data.frame(lme4::VarCorr(model))$sdcor, tolerance = 1e-3)
  expect_equal(mp$SE, c(5.68188, 4.951, 9.773, 0.34887, 0.59977, 0.3494, 1.7238), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(16.713, 37.06178, 36.14261, -0.65336, -0.92243, -0.99569, 24.18612), tolerance = 1e-3)
  expect_equal(
    mp$Parameter,
    c(
      "SD (Days2(-1,3])", "SD (Days2(3,6])", "SD (Days2(6,10])",
      "Cor (Days2(-1,3]~Days2(3,6])", "Cor (Days2(-1,3]~Days2(6,10])",
      "Cor (Days2(3,6]~Days2(6,10])", "SD (Observations)"
    )
  )
})
