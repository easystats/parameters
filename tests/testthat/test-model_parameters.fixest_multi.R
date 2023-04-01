if (requiet("fixest")) {
  set.seed(123)
  iris$x <- rnorm(150)

  test_that("model_parameters.fixest_multi", {
    mod <- feols(c(Petal.Width, Sepal.Width) ~ x + csw0(Petal.Length, Sepal.Length) | Species, iris)
    expect_snapshot(print(model_parameters(mod)))
    expect_snapshot(print(ci(mod)))
  })
  test_that("model_parameters.fixest_multi", {
    mod <- feols(c(Petal.Width, Sepal.Width) ~ x + Petal.Length | Species, iris)
    expect_snapshot(print(model_parameters(mod)))
    expect_snapshot(print(ci(mod)))
  })
  test_that("model_parameters.fixest_multi", {
    mod <- feols(Petal.Width ~ x + csw0(Petal.Length, Sepal.Length) | Species, iris)
    expect_snapshot(print(model_parameters(mod)))
    expect_snapshot(print(ci(mod)))
  })
}
