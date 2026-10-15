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


test_that("satterthwaite default keeps a robust vcov", {
  skip_if_not_installed("clubSandwich")
  data("sleepstudy", package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  out <- model_parameters(
    m,
    effects = "fixed",
    vcov = "CR0",
    vcov_args = list(cluster = sleepstudy$Subject)
  )
  expect_equal(
    out$CI_low,
    out$Coefficient - stats::qt(0.975, out$df_error) * out$SE,
    tolerance = 1e-6
  )
  out_ci <- ci(m, vcov = "CR0", vcov_args = list(cluster = sleepstudy$Subject))
  expect_equal(out_ci$CI_low, out$CI_low, tolerance = 1e-6)
})


test_that("satterthwaite default falls back to wald without the model data", {
  # fit the models in an environment and remove the data afterwards, as with
  # a model that was saved with saveRDS() and loaded in a new session
  e <- new.env()
  local(
    {
      d <- lme4::sleepstudy
      m_lme4 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = d)
      m_lmerTest <- lmerTest::lmer(Reaction ~ Days + (1 | Subject), data = d)
    },
    envir = e
  )
  rm("d", envir = e)

  for (m in list(e$m_lme4, e$m_lmerTest)) {
    expect_message(out <- model_parameters(m, effects = "fixed"), regex = "Satterthwaite")
    expect_equal(
      out$CI_low,
      model_parameters(m, effects = "fixed", ci_method = "residual")$CI_low,
      tolerance = 1e-6
    )
    expect_message(out <- ci(m), regex = "Satterthwaite")
    expect_identical(out, ci(m, method = "wald"))
    expect_message(out <- p_value(m), regex = "Satterthwaite")
    expect_identical(out, p_value(m, method = "wald"))
    expect_silent(model_parameters(m, effects = "fixed", verbose = FALSE))
    expect_error(model_parameters(m, ci_method = "satterthwaite"))
  }
})


test_that("satterthwaite default is skipped without lmerTest", {
  data("sleepstudy", package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  local_mocked_bindings(.check_lmerTest = function(verbose = TRUE) FALSE)
  expect_identical(ci(m), ci(m, method = "wald"))
  expect_identical(p_value(m), p_value(m, method = "wald"))
  expect_identical(
    model_parameters(m, effects = "fixed"),
    model_parameters(m, effects = "fixed", ci_method = "residual")
  )
})
