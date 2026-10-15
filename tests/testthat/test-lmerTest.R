skip_on_cran()

skip_if_not_installed("lmerTest")
skip_if_not_installed("pbkrtest")

data("carrots", package = "lmerTest")
m1 <- lmerTest::lmer(
  Preference ~ sens2 + Homesize + (1 + sens2 | Consumer),
  data = carrots
)

test_that("model_parameters, satterthwaite", {
  params <- model_parameters(m1, effects = "fixed", ci_method = "satterthwaite")
  s <- summary(m1)
  expect_equal(params$df, as.vector(s$coefficients[, "df"]), tolerance = 1e-4)
  expect_equal(params$t, as.vector(s$coefficients[, "t value"]), tolerance = 1e-4)
  expect_equal(params$p, as.vector(s$coefficients[, "Pr(>|t|)"]), tolerance = 1e-4)
})

test_that("model_parameters, kenward", {
  params <- model_parameters(m1, effects = "fixed", ci_method = "kenward")
  s <- summary(m1, ddf = "Kenward-Roger")
  expect_equal(params$df, as.vector(s$coefficients[, "df"]), tolerance = 1e-4)
  expect_equal(params$t, as.vector(s$coefficients[, "t value"]), tolerance = 1e-4)
  expect_equal(params$p, as.vector(s$coefficients[, "Pr(>|t|)"]), tolerance = 1e-4)
})


test_that("mixed-model defaults use satterthwaite for lmerMod", {
  params_default <- model_parameters(m1, effects = "fixed")
  params_satterthwaite <- model_parameters(
    m1,
    effects = "fixed",
    ci_method = "satterthwaite"
  )
  params_wald <- model_parameters(m1, effects = "fixed", ci_method = "wald")

  expect_identical(params_default, params_satterthwaite)
  expect_false(identical(params_default, params_wald))
  expect_true(all(params_wald$p < params_default$p))
  expect_identical(params_wald$t, params_default$t)

  expect_identical(ci(m1), ci(m1, method = "satterthwaite"))
  expect_identical(standard_error(m1), standard_error(m1, method = "satterthwaite"))
  expect_identical(p_value(m1), p_value(m1, method = "satterthwaite"))

  glmer_model <- lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")

  expect_identical(ci(glmer_model), ci(glmer_model, method = "wald"))
  expect_identical(
    standard_error(glmer_model),
    standard_error(glmer_model, method = "wald")
  )
  expect_identical(p_value(glmer_model), p_value(glmer_model, method = "wald"))
})
