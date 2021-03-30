if (require("testthat") &&
  require("parameters") &&
  require("mgcv")) {
  set.seed(123)
  dat <- gamSim(6, n = 200, scale = .2, dist = "poisson")
  m1 <-
    gamm(
      y ~ s(x0) + s(x1) + s(x2),
      family = poisson,
      data = dat,
      random = list(fac = ~1)
    )


  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(2.361598, NA, NA, NA),
      tolerance = 1e-3
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.3476989, NA, NA, NA),
      tolerance = 1e-3
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0, 0, 0),
      tolerance = 1e-3
    )
  })

  mp <- model_parameters(m1)
  test_that("model_parameters", {
    expect_equal(
      mp$Coefficient,
      c(3.0476, NA, NA, NA),
      tolerance = 1e-3
    )
  })

  test_that("model_parameters", {
    expect_equal(
      mp$df,
      c(NA, 3.84696, 3.17389, 8.51855),
      tolerance = 1e-3
    )
  })

  test_that("model_parameters", {
    expect_equal(
      mp$df_error,
      c(183.4606, NA, NA, NA),
      tolerance = 1e-3
    )
  })
}
