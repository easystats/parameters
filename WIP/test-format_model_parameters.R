if (require("testthat") && require("parameters") && require("splines")) {
  data(mtcars)

  m <- lm(mpg ~ qsec:wt + wt:drat, data = mtcars)
  test_that("format_model_parameters-1", {
    expect_equal(unname(format_parameters(m)), c("(Intercept)", "qsec : wt", "wt : drat"))
  })

  m <- lm(mpg ~ qsec : wt + wt / drat, data = mtcars)
  test_that("format_model_parameters-2", {
    expect_equal(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec : wt", "wt : drat"))
  })

  m <- lm(mpg ~ qsec:wt + wt:drat + wt, data = mtcars)
  test_that("format_model_parameters-3", {
    expect_equal(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec : wt", "wt : drat"))
  })

  m <- lm(mpg ~ qsec : wt + wt / drat + wt, data = mtcars)
  test_that("format_model_parameters-4", {
    expect_equal(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec : wt", "wt : drat"))
  })

  m <- lm(mpg ~ qsec * wt + wt : drat + wt, data = mtcars)
  test_that("format_model_parameters-5", {
    expect_equal(unname(format_parameters(m)), c("(Intercept)", "qsec", "wt", "qsec * wt", "wt : drat"))
  })

  m <- lm(mpg ~ wt + qsec + wt:qsec, data = mtcars)
  test_that("format_model_parameters-6", {
    expect_equal(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec", "wt * qsec"))
  })

  m <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  test_that("format_model_parameters-7", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Petal.Length", "Species [versicolor] * Petal.Length", "Species [virginica] * Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species : Petal.Length, data = iris)
  test_that("format_model_parameters-8", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [setosa] : Petal.Length", "Species [versicolor] : Petal.Length",
        "Species [virginica] : Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species / Petal.Length, data = iris)
  test_that("format_model_parameters-9", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Species [setosa] : Petal.Length", "Species [versicolor] : Petal.Length",
        "Species [virginica] : Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species * Petal.Length + Species, data = iris)
  test_that("format_model_parameters-10", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Petal.Length", "Species [versicolor] * Petal.Length", "Species [virginica] * Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species : Petal.Length + Species, data = iris)
  test_that("format_model_parameters-11", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Species [setosa] : Petal.Length", "Species [versicolor] : Petal.Length",
        "Species [virginica] : Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species / Petal.Length + Species, data = iris)
  test_that("format_model_parameters-12", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Species [setosa] : Petal.Length", "Species [versicolor] : Petal.Length",
        "Species [virginica] : Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species * Petal.Length + Petal.Length, data = iris)
  test_that("format_model_parameters-13", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Petal.Length", "Species [versicolor] * Petal.Length", "Species [virginica] * Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species : Petal.Length + Petal.Length, data = iris)
  test_that("format_model_parameters-14", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Petal.Length", "Species [versicolor] : Petal.Length",
        "Species [virginica] : Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species / Petal.Length + Petal.Length, data = iris)
  test_that("format_model_parameters-15", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Petal.Length", "Species [versicolor] * Petal.Length", "Species [virginica] * Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species * Petal.Length + Petal.Length + Species, data = iris)
  test_that("format_model_parameters-16", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Petal.Length", "Species [versicolor] * Petal.Length", "Species [virginica] * Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species : Petal.Length + Petal.Length + Species, data = iris)
  test_that("format_model_parameters-17", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Petal.Length", "Species [versicolor]", "Species [virginica]",
        "Species [versicolor] * Petal.Length", "Species [virginica] * Petal.Length")
    )
  })

  m <- lm(Sepal.Width ~ Species / Petal.Length + Petal.Length + Species, data = iris)
  test_that("format_model_parameters-18", {
    expect_equal(
      unname(format_parameters(m)),
      c("(Intercept)", "Species [versicolor]", "Species [virginica]",
        "Petal.Length", "Species [versicolor] * Petal.Length", "Species [virginica] * Petal.Length")
    )
  })
}
