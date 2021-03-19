if (require("testthat") &&
  require("parameters") &&
  require("mgcv")) {
  set.seed(123)
  dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
  m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(7.771085, NA, NA, NA, NA),
      tolerance = 1e-2
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.1020741, NA, NA, NA, NA),
      tolerance = 1e-2
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0, 0, 0, 0.00196),
      tolerance = 1e-2
    )
  })

  .runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

  if (.runThisTest) {
    mp <- model_parameters(m1)
    test_that("model_parameters", {
      expect_equal(
        mp$Coefficient,
        c(7.97176, NA, NA, NA, NA),
        tolerance = 1e-3
      )
      expect_equal(
        mp$df,
        c(NA, 3.63421, 2.97192, 8.29867, 1.04607),
        tolerance = 1e-3
      )
      expect_equal(
        mp$df_error,
        c(383.04913, NA, NA, NA, NA),
        tolerance = 1e-3
      )
    })
  }
}
