test_that("model_parameters.ggeffects", {
  skip_if_not_installed("ggeffects")
  data(iris)
  mgg <- lm(Sepal.Length ~ Petal.Width + Petal.Length * Species, data = iris)
  model <- ggeffects::ggpredict(mgg, terms = c("Petal.Length", "Species"))
  params <- model_parameters(model)

  expect_named(
    params,
    c("Petal.Length", "Predicted", "CI", "CI_low", "CI_high", "Component")
  )
  expect_snapshot(print(params))
})
