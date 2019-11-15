test_that("format_parameters", {
  model <- lm(Sepal.Length ~ Species * Sepal.Width, data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Species * Sepal.Width * Petal.Length, data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Species / Petal.Length, data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Petal.Length + (Species / Sepal.Width), data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Species / Petal.Length * Sepal.Width, data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Species / (Petal.Length * Sepal.Width), data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Petal.Length + (Species / (Sepal.Width * Petal.Width)), data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2), data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Species + poly(Sepal.Width, 2, raw = TRUE), data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Petal.Length * bs(Petal.Width), data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Petal.Length * bs(Petal.Width, degree = 4), data = iris)
  fp <- format_parameters(model)

  model <- lm(Sepal.Length ~ Petal.Length * ns(Petal.Width, df = 3), data = iris)
  fp <- format_parameters(model)
})
