skip_if_not_installed("lme4")
skip_on_cran()

m1 <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
m2 <- lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")
m3 <- lme4::lmer(wt ~ cyl + mpg + (1 | gear), data = mtcars)

test_that("model_parameters.mixed", {
  params <- model_parameters(m3, keep = "^cyl", effects = "fixed")
  expect_identical(dim(params), c(1L, 10L))
  expect_message({
    params <- model_parameters(m3, keep = "^abc", effects = "fixed")
  })
  expect_identical(dim(params), c(3L, 10L))

  params <- model_parameters(m1, ci_method = "normal", effects = "fixed")
  expect_identical(c(nrow(params), ncol(params)), c(2L, 10L))
  expect_equal(params$CI_high, c(1.6373105660317, 0.554067677205595), tolerance = 1e-3)

  params <- model_parameters(m1, effects = "fixed")
  expect_identical(c(nrow(params), ncol(params)), c(2L, 10L))
  expect_equal(params$CI_high, c(1.68181, 0.56083), tolerance = 1e-3)

  params <- model_parameters(m1, ci = c(0.8, 0.9), ci_method = "normal", effects = "fixed")
  expect_identical(c(nrow(params), ncol(params)), c(2L, 11L))
  expect_equal(params$CI_high_0.8, c(1.29595665381331, 0.502185700948862), tolerance = 1e-3)
  expect_equal(params$CI_high_0.9, c(1.47875781798108, 0.529969433080186), tolerance = 1e-3)

  params <- model_parameters(m1, ci_method = "normal", effects = "fixed")
  lme4_ci <- na.omit(as.data.frame(confint(m1, method = "Wald")))
  expect_equal(params$CI_low, lme4_ci$`2.5 %`, tolerance = 1e-4)

  params <- model_parameters(m1, ci = c(0.8, 0.9), ci_method = "wald", effects = "fixed")
  expect_identical(c(nrow(params), ncol(params)), c(2L, 11L))
  expect_equal(params$CI_high_0.8, c(1.31154, 0.50455), tolerance = 1e-3)
  expect_equal(params$CI_high_0.9, c(1.50707, 0.53427), tolerance = 1e-3)

  params <- model_parameters(m1, ci = c(0.8, 0.9), effects = "fixed")
  expect_identical(c(nrow(params), ncol(params)), c(2L, 11L))
  expect_equal(params$CI_high_0.8, c(1.31154, 0.50455), tolerance = 1e-3)
  expect_equal(params$CI_high_0.9, c(1.50707, 0.53427), tolerance = 1e-3)

  params <- model_parameters(m2, effects = "fixed")
  expect_identical(c(nrow(params), ncol(params)), c(2L, 10L))

  model <- lme4::glmer(vs ~ drat + cyl + (1 | gear), data = mtcars, family = "binomial")
  params <- model_parameters(model, effects = "fixed")
  cs <- coef(summary(model))
  expect_identical(c(nrow(params), ncol(params)), c(3L, 10L))
  expect_named(params, c(
    "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
    "z", "df_error", "p", "Effects"
  ))
  expect_identical(params$Parameter, rownames(cs))
})


test_that("model_parameters.mixed bootstrap", {
  skip_on_os(c("linux", "mac"))
  skip_on_cran()
  set.seed(123)
  suppressWarnings(expect_message(
    {
      params <- model_parameters(m1, bootstrap = TRUE, iterations = 100)
    },
    regex = "only returns"
  ))
  expect_equal(params$Coefficient, c(0.60496, 0.41412), tolerance = 1e-3)
})


test_that("model_parameters.mixed-random", {
  params <- model_parameters(m1, effects = "random", group_level = TRUE)
  expect_identical(c(nrow(params), ncol(params)), c(3L, 9L))
  expect_identical(as.vector(params$Parameter), c("(Intercept)", "(Intercept)", "(Intercept)"))
  expect_identical(as.vector(params$Level), c("3", "4", "5"))
  expect_equal(params$Coefficient, c(0.1692, 0.0566, -0.2259), tolerance = 1e-2)
})

test_that("model_parameters.mixed-ran_pars", {
  params <- model_parameters(m1, effects = "random")
  expect_identical(c(nrow(params), ncol(params)), c(2L, 8L))
  expect_identical(
    as.vector(params$Parameter),
    c("SD (Intercept)", "SD (Observations)")
  )
  expect_equal(params$Coefficient, c(0.27049, 0.59385), tolerance = 1e-2)
})

test_that("model_parameters.mixed-all", {
  params <- model_parameters(m1, effects = "all")
  expect_identical(c(nrow(params), ncol(params)), c(4L, 11L))
  expect_identical(
    as.vector(params$Parameter),
    c("(Intercept)", "cyl", "SD (Intercept)", "SD (Observations)")
  )
  expect_equal(params$Coefficient, c(0.65112, 0.40418, 0.27049, 0.59385), tolerance = 1e-2)
})

test_that("model_parameters.mixed-all_pars", {
  params <- model_parameters(m1, effects = "all", group_level = TRUE)
  expect_identical(c(nrow(params), ncol(params)), c(5L, 12L))
  expect_identical(
    as.vector(params$Parameter),
    c("(Intercept)", "cyl", "(Intercept)", "(Intercept)", "(Intercept)")
  )
  expect_equal(as.vector(params$Level), c(NA, NA, "3", "4", "5"), ignore_attr = TRUE)
  expect_equal(
    params$Coefficient,
    c(0.65112, 0.40418, 0.16923, 0.05663, -0.22586),
    tolerance = 1e-2
  )
})


data("qol_cancer")
qol_cancer <- cbind(
  qol_cancer,
  demean(qol_cancer, select = c("phq4", "QoL"), by = "ID")
)
model <- lme4::lmer(
  QoL ~ time + phq4_within + phq4_between + (1 | ID),
  data = qol_cancer
)

test_that("model_parameters.mixed", {
  mp <- model_parameters(model, effects = "fixed", wb_component = TRUE)
  mp2 <- model_parameters(model, effects = "fixed", wb_component = FALSE)
  expect_identical(mp$Component, c("rewb-contextual", "rewb-contextual", "within", "between"))
  expect_null(mp2$Component)
})


test_that("print-model_parameters-1", {
  expect_snapshot(model_parameters(model, effects = "fixed", wb_component = TRUE))
  expect_snapshot(model_parameters(model, effects = "fixed", wb_component = FALSE))
})

test_that("print-model_parameters-2", {
  skip_if_not_installed("merDeriv")
  expect_snapshot(model_parameters(m1, effects = "all", wb_component = TRUE))
  expect_snapshot(model_parameters(m1, effects = "all", wb_component = FALSE))

  expect_snapshot(model_parameters(m1, effects = "fixed", include_info = TRUE, wb_component = TRUE))
  expect_snapshot(model_parameters(m1, effects = "fixed", include_info = TRUE, wb_component = FALSE))
})
