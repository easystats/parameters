if (require("testthat") && require("VGAM") && require("parameters")) {
  data("pneumo")
  data("hunua")

  set.seed(123)
  pneumo <- transform(pneumo, let = log(exposure.time))

  m1 <- vgam(
    cbind(normal, mild, severe) ~ s(let) + exposure.time,
    cumulative(parallel = TRUE),
    data = pneumo,
    trace = FALSE
  )

  set.seed(123)
  hunua$x <- rnorm(nrow(hunua))
  m2 <- vgam(agaaus ~ s(altitude, df = 2) + s(x) + beitaw + corlae, binomialff, data = hunua)

  test_that("model_parameters.truncreg", {
    params <- model_parameters(m1)
    expect_equal(params$Coefficient, as.vector(m1@coefficients), tolerance = 1e-3)
    expect_equal(params$Parameter, c("(Intercept):1", "(Intercept):2", "exposure.time", "s(let)"))
    expect_equal(params$df, c(NA, NA, NA, 2.65007), tolerance = 1e-3)
    expect_equal(as.vector(na.omit(params$df)), as.vector(m1@nl.df), tolerance = 1e-3)
  })

  test_that("model_parameters.truncreg", {
    params <- model_parameters(m2)
    expect_equal(params$Coefficient, as.vector(m2@coefficients), tolerance = 1e-3)
    expect_equal(params$Parameter, c("(Intercept)", "beitaw", "corlae", "s(altitude, df = 2)", "s(x)"))
    expect_equal(params$df, c(NA, NA, NA, 0.82686, 2.8054), tolerance = 1e-3)
    expect_equal(as.vector(na.omit(params$df)), as.vector(m2@nl.df), tolerance = 1e-3)
  })
}
