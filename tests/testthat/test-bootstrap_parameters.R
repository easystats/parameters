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
