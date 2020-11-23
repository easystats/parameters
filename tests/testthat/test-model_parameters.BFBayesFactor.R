if (require("testthat") &&
  require("parameters") &&
  require("BayesFactor") &&
  require("logspline")) {
  .runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

  if (.runThisTest) {
    test_that("model_parameters.BFBayesFactor", {
      skip_on_cran()
      model <- BayesFactor::ttestBF(iris$Sepal.Width, iris$Petal.Length, paired = TRUE)
      testthat::expect_equal(parameters::model_parameters(model)$BF, 492.770, tolerance = 2)
    })
  }

  test_that("model_parameters.BFBayesFactor", {
    model <- BayesFactor::correlationBF(iris$Sepal.Width, iris$Petal.Length)
    testthat::expect_equal(parameters::model_parameters(model)$BF, 348853.6, tolerance = 10)
  })

  test_that("model_parameters.BFBayesFactor", {
    set.seed(123)
    model <- BayesFactor::anovaBF(Sepal.Length ~ Species, data = iris)
    testthat::expect_equal(parameters::model_parameters(model)$Median, c(5.8431, -0.8266, 0.092, 0.734, 0.2681, 2.0415), tolerance = 2)
  })

  df <- mtcars
  df$gear <- as.factor(df$gear)
  df$am <- as.factor(df$am)

  if (.runThisTest) {
    test_that("model_parameters.BFBayesFactor", {
      skip_on_cran()
      model <- BayesFactor::ttestBF(formula = mpg ~ am, data = df)
      expect_equal(model_parameters(model)$BF, 86, tolerance = 2)
    })
  }

  test_that("model_parameters.BFBayesFactor", {
    set.seed(123)
    model <- BayesFactor::anovaBF(mpg ~ gear * am, data = df, )
    expect_equal(model_parameters(model)$Median, c(20.69277, -3.24014, 3.24014, 25.28076, 0.79331), tolerance = 2)
  })

  if (.runThisTest) {
    data(raceDolls)
    bf <- contingencyTableBF(raceDolls, sampleType = "indepMulti", fixedMargin = "cols")
    mp <- model_parameters(bf)

    test_that("model_parameters.BFBayesFactor", {
      expect_equal(colnames(mp), "BF")
    })

    result <- anovaBF(RT ~ shape*color + ID, data = puzzles, whichRandom = "ID",
                      whichModels = 'top', progress = FALSE)
    mp <- model_parameters(result, verbose = FALSE)

    test_that("model_parameters.BFBayesFactor", {
      expect_equal(colnames(mp), c("Parameter", "Median", "CI_low", "CI_high", "pd", "ROPE_Percentage",
                                   "Prior_Distribution", "Prior_Location", "Prior_Scale", "Effects",
                                   "Component", "BF"))
      expect_equal(mp$Effects, c("fixed", "fixed", "fixed", "fixed", "fixed", "random", "random",
                                 "random", "random", "random", "random", "random", "random", "random",
                                 "random", "random", "random", "fixed", "fixed", "fixed", "fixed"))
    })
  }
}
