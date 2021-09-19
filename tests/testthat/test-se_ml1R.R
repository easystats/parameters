if (requiet("testthat") && requiet("parameters") && requiet("lme4")) {
  data(iris)
  model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
  test_that("se_ml1", {
    expect_equal(se_ml1(model)$SE, c(1.19664, 0.31413), tolerance = 1e-3)
  })
}
