if (require("testthat") &&
  require("parameters") &&
  require("tripack") &&
  require("quantreg")) {

  # rqss ---------

  data("CobarOre")
  set.seed(123)
  CobarOre$w <- rnorm(nrow(CobarOre))
  m1 <- rqss(z ~ w + qss(cbind(x, y), lambda = .08), data = CobarOre)

  mp <- model_parameters(m1)
  test_that("mp_rqss", {
    expect_identical(mp$Parameter, c("(Intercept)", "w"))
    expect_equal(mp$Coefficient, c(17.63057, 1.12506), tolerance = 1e-3)
  })



  # rq ---------

  data(stackloss)
  m1 <- rq(stack.loss ~ Air.Flow + Water.Temp, data = stackloss, tau = .25)

  mp <- suppressWarnings(model_parameters(m1))
  test_that("mp_rq", {
    expect_identical(mp$Parameter, c("(Intercept)", "Air.Flow", "Water.Temp"))
    expect_equal(mp$Coefficient, c(-36, 0.5, 1), tolerance = 1e-3)
  })



  # crq ---------

  set.seed(123)
  n <- 200
  x <- rnorm(n)
  y <- 5 + x + rnorm(n)
  c <- 4 + x + rnorm(n)
  d <- (y > c)

  dat <- data.frame(y, x, c, d)
  m1 <- crq(survival::Surv(pmax(y, c), d, type = "left") ~ x, method = "Portnoy", data = dat)

  mp <- model_parameters(m1)
  test_that("mp_rq", {
    expect_identical(mp$Parameter, c("(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x", "(Intercept)", "x"))
    expect_equal(mp$Coefficient, c(4.26724, 0.97534, 4.84961, 0.92638, 5.21843, 0.98038, 5.91301, 0.97382), tolerance = 1e-3)
  })
}
