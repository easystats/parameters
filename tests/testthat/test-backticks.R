if (require("testthat") && require("parameters")) {
  context("backticks")

  data(iris)
  iris$`a m` <- iris$Species
  iris$`Sepal Width` <- iris$Sepal.Width
  m1 <- lm(`Sepal Width` ~ Petal.Length + `a m` * log(Sepal.Length), data = iris)
  m2 <- lm(Sepal.Width ~ Petal.Length + Species * log(Sepal.Length), data = iris)

  test_that("standard_error, backticks", {
    expect_equal(
      standard_error(m1)$Parameter,
      c("(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
        "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)")
    )
    expect_equal(
      standard_error(m2)$Parameter,
      c("(Intercept)", "Petal.Length", "Speciesversicolor", "Speciesvirginica",
        "log(Sepal.Length)", "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)")
    )
  })

  test_that("standard_error, backticks", {
    expect_equal(
      standard_error(m1)$Parameter,
      c("(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
        "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)")
    )
    expect_equal(
      standard_error(m2)$Parameter,
      c("(Intercept)", "Petal.Length", "Speciesversicolor", "Speciesvirginica",
        "log(Sepal.Length)", "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)")
    )
  })
}