skip_if_not_installed("withr")

# make sure we have the correct interaction mark for tests
withr::with_options(
  list(parameters_interaction = "*"),
  {
    test_that("format_model_parameters-1", {
      m <- lm(mpg ~ qsec:wt + wt:drat, data = mtcars)
      expect_identical(unname(format_parameters(m)), c("(Intercept)", "qsec * wt", "wt * drat"))
    })

    test_that("format_model_parameters-2", {
      m <- lm(mpg ~ qsec:wt + wt / drat, data = mtcars)
      expect_identical(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec * wt", "wt * drat"))
    })

    test_that("format_model_parameters-3", {
      m <- lm(mpg ~ qsec:wt + wt:drat + wt, data = mtcars)
      expect_identical(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec * wt", "wt * drat"))
    })

    test_that("format_model_parameters-4", {
      m <- lm(mpg ~ qsec:wt + wt / drat + wt, data = mtcars)
      expect_identical(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec * wt", "wt * drat"))
    })

    test_that("format_model_parameters-5", {
      m <- lm(mpg ~ qsec * wt + wt:drat + wt, data = mtcars)
      expect_identical(unname(format_parameters(m)), c("(Intercept)", "qsec", "wt", "qsec * wt", "wt * drat"))
    })

    test_that("format_model_parameters-6", {
      m <- lm(mpg ~ wt + qsec + wt:qsec, data = mtcars)
      expect_identical(unname(format_parameters(m)), c("(Intercept)", "wt", "qsec", "wt * qsec"))
    })

    test_that("format_model_parameters-7", {
      m <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-8", {
      m <- lm(Sepal.Width ~ Species:Petal.Length, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [setosa] * Petal Length",
          "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-9", {
      m <- lm(Sepal.Width ~ Species / Petal.Length, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Species [setosa] * Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-10", {
      m <- lm(Sepal.Width ~ Species * Petal.Length + Species, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-11", {
      m <- lm(Sepal.Width ~ Species:Petal.Length + Species, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Species [setosa] * Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-12", {
      m <- lm(Sepal.Width ~ Species / Petal.Length + Species, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Species [setosa] * Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-13", {
      m <- lm(Sepal.Width ~ Species * Petal.Length + Petal.Length, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length", "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-14", {
      m <- lm(Sepal.Width ~ Species:Petal.Length + Petal.Length, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-15", {
      m <- lm(Sepal.Width ~ Species / Petal.Length + Petal.Length, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length", "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-16", {
      m <- lm(Sepal.Width ~ Species * Petal.Length + Petal.Length + Species, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length", "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-17", {
      m <- lm(Sepal.Width ~ Species:Petal.Length + Petal.Length + Species, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Petal Length", "Species [versicolor]", "Species [virginica]",
          "Species [versicolor] * Petal Length", "Species [virginica] * Petal Length"
        )
      )
    })

    test_that("format_model_parameters-18", {
      m <- lm(Sepal.Width ~ Species / Petal.Length + Petal.Length + Species, data = iris)
      expect_identical(
        unname(format_parameters(m)),
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length", "Species [virginica] * Petal Length"
        )
      )
    })
  }
)

skip_if_not_installed("lme4")
skip_if_not_installed("glmmTMB")

test_that("format, compare_parameters, mixed models", {
  data(mtcars)
  data(Salamanders, package = "glmmTMB")
  model1 <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
  model2 <- glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~mined,
    family = poisson(),
    data = Salamanders
  )
  out <- compare_parameters(model1, model2, effects = "all", component = "all")
  f <- format(out)
  expect_length(f, 3)
  f <- format(out, format = "html")
  expect_identical(
    f$Component,
    c(
      "Fixed Effects", "Fixed Effects", "Fixed Effects", "Fixed Effects",
      "Fixed Effects", "Fixed Effects", "Fixed Effects", "Fixed Effects",
      "Fixed Effects", "Fixed Effects (Zero-Inflation Component)",
      "Fixed Effects (Zero-Inflation Component)", "Random Effects",
      "Random Effects", "Random Effects"
    )
  )
})
