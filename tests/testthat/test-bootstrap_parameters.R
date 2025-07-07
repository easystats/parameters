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
    out,
    c(
      6.3, 4.8, 7, 6, 6.3, 6.7, 6.6, 6.6, 6.1, 6.1, 6.3, 6.3, 6.1,
      7.2, 5.9, 5.6, 5.7, 6.8, 6.7, 6.4
    ),
    tolerance = 1e-2
  )
})
