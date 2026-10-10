skip_on_cran()
skip_if_not_installed("boot")

test_that("bootstrap_parameters.bootstrap_model", {
  data(iris)
  m_draws <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  set.seed(123)
  draws <- bootstrap_model(m_draws)
  draws$lin_comb <- draws$Sepal.Width - draws$Petal.Length
  out <- bootstrap_parameters(draws)
  expect_snapshot(print(out))
})

test_that("bootstrap_model intercept-only", {
  y <- 1:10
  mod <- lm(y ~ 1)
  set.seed(123)
  out <- bootstrap_model(mod, iterations = 20)
  expect_equal(
    as.numeric(out),
    c(
      6.3, 4.8, 7, 6, 6.3, 6.7, 6.6, 6.6, 6.1, 6.1, 6.3, 6.3, 6.1,
      7.2, 5.9, 5.6, 5.7, 6.8, 6.7, 6.4
    ),
    tolerance = 1e-2
  )
})

test_that("bootstrap_model semiparametric for merMod", {
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  set.seed(123)
  out <- bootstrap_model(m, iterations = 20, type = "semiparametric")
  expect_named(out, c("(Intercept)", "Days"))
  expect_identical(nrow(out), 20L)
  set.seed(123)
  out <- bootstrap_parameters(m, iterations = 20, type = "semiparametric")
  expect_identical(out$Parameter, c("(Intercept)", "Days"))
})

test_that("bootstrap_model semiparametric errors for glmmTMB", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  expect_error(
    bootstrap_model(m, iterations = 5, type = "semiparametric"),
    regex = "not available for models from"
  )
})
