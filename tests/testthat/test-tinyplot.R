test_that("tinyplot.parameters_model works", {
  skip_if_not_installed("tinyplot", minimum_version = "0.7.0")

  model <- lm(mpg ~ wt + cyl + gear, data = mtcars)
  result <- model_parameters(model)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(tinyplot::tinyplot(result))
  expect_no_error(tinyplot::tinyplot(result, sort = TRUE, zero = FALSE, flip = FALSE))
  out <- withVisible(tinyplot::tinyplot(result))
  expect_false(out$visible)
  expect_identical(out$value, result)
})

test_that("tinyplot.parameters_model informs about dropped effects and components", {
  skip_if_not_installed("tinyplot", minimum_version = "0.7.0")
  skip_if_not_installed("lme4")
  skip_if_not_installed("pscl")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  data("sleepstudy", package = "lme4")
  model <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  result <- model_parameters(model)
  expect_message(tinyplot::tinyplot(result), regexp = "fixed effects")

  data("bioChemists", package = "pscl")
  model <- pscl::zeroinfl(art ~ fem + mar | kid5, data = bioChemists)
  result <- model_parameters(model)
  expect_message(tinyplot::tinyplot(result), regexp = "conditional component")
})

test_that("tinyplot.parameters_model handles special models", {
  skip_if_not_installed("tinyplot", minimum_version = "0.7.0")
  skip_if_not_installed("nnet")
  skip_if_not_installed("MASS")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  # multinomial models have duplicated parameter names across response levels
  model <- nnet::multinom(Species ~ Sepal.Width, data = iris, trace = FALSE)
  result <- model_parameters(model)
  expect_no_error(tinyplot::tinyplot(result))

  # ordinal models have components, but no conditional component
  model <- MASS::polr(Species ~ Sepal.Width, data = iris)
  result <- model_parameters(model)
  expect_no_error(tinyplot::tinyplot(result))
})

test_that("tinyplot.parameters_model snapshots", {
  skip_on_cran()
  skip_if_not_installed("tinyplot", minimum_version = "0.7.0")
  skip_if_not_installed("vdiffr")

  model <- lm(mpg ~ wt + cyl + gear, data = mtcars)
  result <- model_parameters(model)
  vdiffr::expect_doppelganger(
    "tinyplot-forest 1",
    function() tinyplot::tinyplot(result)
  )
  vdiffr::expect_doppelganger(
    "tinyplot-forest 2",
    function() tinyplot::tinyplot(result, sort = TRUE)
  )

  model <- glm(am ~ wt + hp, data = mtcars, family = "binomial")
  result <- model_parameters(model, exponentiate = TRUE)
  vdiffr::expect_doppelganger(
    "tinyplot-forest exponentiated",
    function() tinyplot::tinyplot(result)
  )
})
