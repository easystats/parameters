if (require("testthat") && require("parameters")) {
  data(iris)
  dat <- iris
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type default contrasts", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "factor", "factor"))
    expect_equal(p_type$Level, c(NA, "versicolor", "virginica"))
  })

  data(iris)
  dat <- iris
  dat$Species <- as.ordered(dat$Species)
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type ordered factor", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "ordered", "ordered"))
    expect_equal(p_type$Level, c(NA, "[linear]", "[quadratic]"))
  })

  data(iris)
  dat <- iris
  dat$Species <- as.ordered(dat$Species)
  contrasts(dat$Species) <- contr.treatment(3)
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type ordered factor", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "factor", "factor"))
    expect_equal(p_type$Level, c(NA, "2", "3"))
  })

  data(iris)
  dat <- iris
  contrasts(dat$Species) <- contr.poly(3)
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type poly contrasts", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "factor", "factor"))
    expect_equal(p_type$Level, c(NA, ".L", ".Q"))
  })

  data(iris)
  dat <- iris
  contrasts(dat$Species) <- contr.treatment(3)
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type treatment contrasts", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "factor", "factor"))
    expect_equal(p_type$Level, c(NA, "2", "3"))
  })

  data(iris)
  dat <- iris
  contrasts(dat$Species) <- contr.sum(3)
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type sum contrasts", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "factor", "factor"))
    expect_equal(p_type$Level, c(NA, "1", "2"))
  })

  data(iris)
  dat <- iris
  contrasts(dat$Species) <- contr.helmert(3)
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type helmert contrasts", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "factor", "factor"))
    expect_equal(p_type$Level, c(NA, "1", "2"))
  })

  data(iris)
  dat <- iris
  contrasts(dat$Species) <- contr.SAS(3)
  m <- lm(Sepal.Length ~ Species, data = dat)
  test_that("parameters_type SAS contrasts", {
    p_type <- parameters_type(m)
    expect_equal(p_type$Type, c("intercept", "factor", "factor"))
    expect_equal(p_type$Level, c(NA, "1", "2"))
  })
}
