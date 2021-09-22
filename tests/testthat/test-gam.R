if (requiet("testthat") &&
  requiet("parameters") &&
  requiet("mgcv")) {
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

    test_that("print model_parameters", {
      out <- utils::capture.output(print(mp))
      expect_equal(
        out,
        c(
          "# Fixed Effects",
          "",
          "Parameter   | Coefficient |   SE |       95% CI | t(383.05) |      p",
          "--------------------------------------------------------------------",
          "(Intercept) |        7.97 | 0.10 | [7.77, 8.17] |     78.10 | < .001",
          "",
          "# Smooth Terms",
          "",
          "Parameter        |     F |   df |      p",
          "----------------------------------------",
          "Smooth term (x0) | 10.53 | 3.63 | < .001",
          "Smooth term (x1) | 87.44 | 2.97 | < .001",
          "Smooth term (x2) | 72.49 | 8.30 | < .001",
          "Smooth term (x3) |  9.58 | 1.05 | 0.002 "
        )
      )
    })
  }
}
