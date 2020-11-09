.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (require("testthat") && require("parameters")) {
    data(iris)
    iris$`a m` <<- iris$Species
    iris$`Sepal Width` <<- iris$Sepal.Width
    m1 <- lm(`Sepal Width` ~ Petal.Length + `a m` * log(Sepal.Length), data = iris)
    m2 <- lm(Sepal.Width ~ Petal.Length + Species * log(Sepal.Length), data = iris)

    test_that("standard_error, backticks", {
      expect_equal(
        standard_error(m1)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
          "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
        )
      )
      expect_equal(
        standard_error(m2)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "Speciesversicolor", "Speciesvirginica",
          "log(Sepal.Length)", "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)"
        )
      )
    })


    test_that("ci, backticks", {
      expect_equal(
        ci(m1)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
          "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
        )
      )
      expect_equal(
        ci(m2)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "Speciesversicolor", "Speciesvirginica",
          "log(Sepal.Length)", "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)"
        )
      )
      expect_equal(
        ci_wald(m1)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
          "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
        )
      )
      expect_equal(
        ci_wald(m2)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "Speciesversicolor", "Speciesvirginica",
          "log(Sepal.Length)", "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)"
        )
      )
    })


    test_that("p, backticks", {
      expect_equal(
        p_value(m1)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
          "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
        )
      )
      expect_equal(
        p_value(m2)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "Speciesversicolor", "Speciesvirginica",
          "log(Sepal.Length)", "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)"
        )
      )
    })


    test_that("model_parameters, backticks", {
      expect_equal(
        model_parameters(m1)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "a mversicolor", "a mvirginica",
          "log(Sepal.Length)", "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
        )
      )
      expect_equal(
        model_parameters(m2)$Parameter,
        c(
          "(Intercept)", "Petal.Length", "Speciesversicolor", "Speciesvirginica",
          "log(Sepal.Length)", "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)"
        )
      )
    })


    test_that("model_parameters-2, backticks", {
      expect_equal(
        model_parameters(select_parameters(m1))$Parameter,
        c(
          "(Intercept)", "a mversicolor", "a mvirginica", "log(Sepal.Length)",
          "a mversicolor:log(Sepal.Length)", "a mvirginica:log(Sepal.Length)"
        )
      )
      expect_equal(
        model_parameters(select_parameters(m2))$Parameter,
        c(
          "(Intercept)", "Speciesversicolor", "Speciesvirginica", "log(Sepal.Length)",
          "Speciesversicolor:log(Sepal.Length)", "Speciesvirginica:log(Sepal.Length)"
        )
      )
    })
  }
}
