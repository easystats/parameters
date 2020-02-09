.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"

if (.runThisTest || Sys.getenv("USER") == "travis") {
  if (require("testthat") &&
    require("parameters") &&
    require("glmmTMB")) {

    data("fish")

    m1 <- glmmTMB(
      count ~ child + camper + (1 | persons),
      ziformula = ~ child + camper + (1 | persons),
      data = fish,
      family = truncated_poisson()
    )

    m2 <- glmmTMB(
      count ~ child + camper + (1 | persons),
      data = fish,
      family = poisson()
    )

    test_that("ci", {
      expect_equal(
        ci(m1)$CI_low,
        c(0.33067, -1.32402, 0.55037, -1.66786, 1.44667, -1.64177),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m1, component = "cond")$CI_low,
        c(0.33067, -1.32402, 0.55037),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m1, component = "zi")$CI_low,
        c(-1.66786, 1.44667, -1.64177),
        tolerance = 1e-3
      )

      expect_equal(
        ci(m2)$CI_low,
        c(-0.47982, -1.85096, 0.76044),
        tolerance = 1e-3
      )
      expect_equal(
        ci(m2, component = "cond")$CI_low,
        c(-0.47982, -1.85096, 0.76044),
        tolerance = 1e-3
      )

      expect_null(ci(m2, component = "zi"))
    })



    test_that("se", {
      expect_equal(
        standard_error(m1)$SE,
        c(0.47559, 0.09305, 0.09346, 0.65229, 0.3099, 0.32324),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m1, component = "cond")$SE,
        c(0.47559, 0.09305, 0.09346),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m1, component = "zi")$SE,
        c(0.65229, 0.3099, 0.32324),
        tolerance = 1e-3
      )

      expect_equal(
        standard_error(m2)$SE,
        c(0.62127, 0.08128, 0.08915),
        tolerance = 1e-3
      )
      expect_equal(
        standard_error(m2, component = "cond")$SE,
        c(0.62127, 0.08128, 0.08915),
        tolerance = 1e-3
      )

      expect_null(standard_error(m2, component = "zi"))
    })


    test_that("p_value", {
      expect_equal(
        p_value(m1)$p,
        c(0.00792, 0, 0, 0.55054, 0, 0.00181),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m1, component = "cond")$p,
        c(0.00792, 0, 0),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m1, component = "zi")$p,
        c(0.55054, 0, 0.00181),
        tolerance = 1e-3
      )

      expect_equal(
        p_value(m2)$p,
        c(0.23497, 0, 0),
        tolerance = 1e-3
      )
      expect_equal(
        p_value(m2, component = "cond")$p,
        c(0.23497, 0, 0),
        tolerance = 1e-3
      )

      expect_null(p_value(m2, component = "zi"))
    })


    test_that("model_parameters", {
      expect_equal(
        model_parameters(m1)$Coefficient,
        c(1.2628, -1.14165, 0.73354, -0.38939, 2.05407, -1.00823),
        tolerance = 1e-3
      )
      expect_equal(
        model_parameters(m2)$Coefficient,
        c(0.73785, -1.69166, 0.93516),
        tolerance = 1e-3
      )
    })
  }
}
