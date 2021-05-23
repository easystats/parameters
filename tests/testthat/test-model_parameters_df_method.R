if (require("testthat") &&
  require("parameters") &&
  require("lmerTest") &&
  require("pbkrtest") &&
  require("lme4")) {
  data("mtcars")
  mtcars$cyl <- as.factor(mtcars$cyl)
  model <- suppressMessages(lme4::lmer(mpg ~ as.factor(gear) * hp + as.factor(am) + wt + (1 | cyl), data = mtcars))
  model2 <- suppressMessages(lmerTest::lmer(mpg ~ as.factor(gear) * hp + as.factor(am) + wt + (1 | cyl), data = mtcars))

  mp1 <- model_parameters(model, digits = 5, effects = "fixed")
  mp2 <- model_parameters(model, digits = 5, df_method = "s", effects = "fixed")
  mp3 <- model_parameters(model, digits = 5, df_method = "k", effects = "fixed")

  test_that("model_parameters, df_method wald", {
    expect_equal(mp1$SE, c(2.77457, 3.69574, 3.521, 0.01574, 1.58514, 0.86316, 0.02973, 0.01668), tolerance = 1e-3)
    expect_equal(mp1$df, c(22, 22, 22, 22, 22, 22, 22, 22), tolerance = 1e-3)
    expect_equal(mp1$p, c(0, 0.00068, 0.12872, 0.15695, 0.846, 0.00224, 0.00029, 0.31562), tolerance = 1e-3)
    expect_equal(mp1$CI_low, c(24.86326, 5.31796, -1.5521, -0.05313, -2.79893, -4.33015, -0.16595, -0.04943), tolerance = 1e-3)
  })

  test_that("model_parameters, df_method satterthwaite", {
    expect_equal(mp2$SE, c(2.77457, 3.69574, 3.521, 0.01574, 1.58514, 0.86316, 0.02973, 0.01668), tolerance = 1e-3)
    expect_equal(mp2$df, c(24, 24, 24, 24, 24, 24, 24, 24), tolerance = 1e-3)
    expect_equal(mp2$p, c(0, 0.00236, 0.14179, 0.16979, 0.84763, 0.00542, 0.00136, 0.32563), tolerance = 1e-3)
    expect_equal(mp2$CI_low, c(24.86326, 5.31796, -1.5521, -0.05313, -2.79893, -4.33015, -0.16595, -0.04943), tolerance = 1e-3)
  })

  test_that("model_parameters, df_method kenward", {
    expect_equal(mp3$SE, c(2.97608, 6.10454, 3.98754, 0.02032, 1.60327, 0.91599, 0.05509, 0.01962), tolerance = 1e-3)
    expect_equal(mp3$df, c(19.39553, 5.27602, 23.57086, 8.97297, 22.7421, 23.76299, 2.72622, 22.82714), tolerance = 1e-3)
    expect_equal(mp3$p, c(0, 0.09176, 0.19257, 0.30147, 0.84942, 0.00828, 0.15478, 0.40248), tolerance = 1e-3)
    expect_equal(mp3$CI_low, c(24.08091, -2.887, -2.88887, -0.06828, -3.01082, -4.5299, -0.29339, -0.05735), tolerance = 1e-3)
  })

  test_that("model_parameters, satterthwaite compare", {
    s <- summary(model2)
    expect_equal(mp2$df, as.vector(s$coefficients[, "df"]), tolerance = 1e-4)
    expect_equal(mp2$t, as.vector(s$coefficients[, "t value"]), tolerance = 1e-4)
    expect_equal(mp2$p, as.vector(s$coefficients[, "Pr(>|t|)"]), tolerance = 1e-4)
    expect_equal(mp2$SE, as.vector(s$coefficients[, "Std. Error"]), tolerance = 1e-4)
  })

  test_that("model_parameters, satterthwaite compare", {
    s <- summary(model2, ddf = "Kenward-Roger")
    expect_equal(mp3$df, as.vector(s$coefficients[, "df"]), tolerance = 1e-4)
    expect_equal(mp3$t, as.vector(s$coefficients[, "t value"]), tolerance = 1e-4)
    expect_equal(mp3$p, as.vector(s$coefficients[, "Pr(>|t|)"]), tolerance = 1e-4)
    expect_equal(mp3$SE, as.vector(s$coefficients[, "Std. Error"]), tolerance = 1e-4)
  })


  model <- lm(mpg ~ as.factor(gear) * hp + as.factor(am) + wt, data = mtcars)
  test_that("model_parameters, df_method-lm", {
    expect_s3_class(model_parameters(model), "parameters_model")
    expect_s3_class(model_parameters(model, df_method = "kenward"), "parameters_model")
  })

  unloadNamespace("afex")
  unloadNamespace("lmerTest")
}
