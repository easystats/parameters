if (require("testthat") &&
  require("parameters") &&
  require("mgcv")) {

  dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
  m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(7.49622189706408, NA, NA, NA, NA),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.103747577565001, NA, NA, NA, NA),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0, 0, 0, 0.96301),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(7.70019, 2.51662, 2.01425, 8.32417, 1),
      tolerance = 1e-4
    )
  })
}
