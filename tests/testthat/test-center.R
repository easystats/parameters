.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (require("testthat") && require("parameters")) {
    data(iris)
    data(mtcars)
    z <- center(iris$Sepal.Width)
    test_that("center", {
      expect_equal(z, iris$Sepal.Width - mean(iris$Sepal.Width))
    })
    z <- center(mtcars$hp, robust = TRUE)
    test_that("center, robust", {
      expect_equal(z, mtcars$hp - median(mtcars$hp))
    })
  }

  z <- center(iris, select = "Sepal.Width")
  test_that("center, select", {
    expect_equal(z$Sepal.Width, iris$Sepal.Width - mean(iris$Sepal.Width))
  })

  z <- center(iris, select = "Species")
  test_that("center, factors", {
    expect_equal(z$Species, iris$Species)
  })

  z <- center(iris, select = "Species", force = TRUE)
  v <- as.numeric(iris$Species)
  test_that("center, force factors", {
    expect_equal(z$Species, v - median(v))
  })

  test_that("center, all na", {
    z <- center(c(NA, NA, NA))
    expect_equal(z, as.numeric(c(NA, NA, NA)))
  })
}
