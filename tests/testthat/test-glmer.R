skip_on_cran()
skip_if_not_installed("lme4")
data("cbpp", package = "lme4")

set.seed(123)
model <- lme4::glmer(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp,
  family = binomial(),
  nAGQ = 0
)
params <- model_parameters(model, effects = "fixed")

test_that("model_parameters.glmer", {
  expect_equal(params$SE, c(0.22758, 0.30329, 0.32351, 0.42445), tolerance = 1e-2)
})

test_that("print model_parameters", {
  skip_if_not_installed("withr")
  skip_if_not_installed("merDeriv")
  withr::local_options(list(
    parameters_exponentiate = TRUE,
    parameters_warning_exponentiate = TRUE
  ))
  expect_snapshot(params)

  suppressMessages({
    mp <- model_parameters(model, effects = "all", exponentiate = TRUE)
  })
  expect_snapshot(mp)

  set.seed(123)
  model <- lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial(),
    nAGQ = 2
  )
  mp <- model_parameters(model, effects = "all")
  expect_snapshot(mp)
})


test_that("model_parameters.glmer ml1", {
  params <- model_parameters(model, ci_method = "ml1", effects = "fixed")
  expect_equal(params$SE, c(0.22758, 0.30329, 0.32351, 0.42445), tolerance = 1e-2)
  expect_equal(params$df, c(54, 54, 54, 54), tolerance = 1e-2)
})

test_that("model_parameters.glmer betwithin", {
  params <- model_parameters(model, ci_method = "betwithin", effects = "fixed")
  expect_equal(params$SE, c(0.23009, 0.30433, 0.32476, 0.42632), tolerance = 1e-2)
  expect_equal(params$df, c(822, 822, 822, 822), tolerance = 1e-2)
})

set.seed(123)
cbpp$time <- runif(nrow(cbpp), 1, 4)
model <- lme4::glmer(
  cbind(incidence, size - incidence) ~ period + time + (1 + time | herd),
  data = cbpp,
  family = binomial(),
  nAGQ = 0
)

test_that("model_parameters.glmer", {
  params <- model_parameters(model, effects = "fixed")
  expect_equal(params$SE, c(0.66539, 0.36178, 0.36223, 0.45528, 0.2379), tolerance = 1e-2)
})

test_that("model_parameters.glmer ml1", {
  params <- model_parameters(model, ci_method = "ml1", effects = "fixed")
  expect_equal(params$SE, c(0.66539, 0.36178, 0.36223, 0.45528, 0.2379), tolerance = 1e-2)
  expect_equal(params$df, c(53, 53, 53, 53, 53), tolerance = 1e-2)
})

test_that("model_parameters.glmer betwithin", {
  params <- model_parameters(model, ci_method = "betwithin", effects = "fixed")
  expect_equal(params$SE, c(0.66539, 0.36178, 0.36223, 0.45528, 0.2379), tolerance = 1e-2)
  expect_equal(params$df, c(821, 821, 821, 821, 9), tolerance = 1e-2)
})

test_that("robust SE/VCOV, using FPC", {
  skip_if(packageVersion("insight") <= "1.5.0")
  data(sleepstudy, package = "lme4")
  model <- lme4::lmer(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
  out <- standard_error(model, vcov = "fpc", vcov_args = list(population_size = 500))
  expect_equal(
    out$SE,
    sqrt(diag(insight::get_varcov(
      model,
      vcov = "fpc",
      vcov_args = list(population_size = 500)
    ))),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
  out <- model_parameters(model, vcov = "fpc", vcov_args = list(population_size = 500))
  expect_snapshot(print(out, table_width = Inf))
})
