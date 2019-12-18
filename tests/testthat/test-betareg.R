if (Sys.getenv("USER") != "travis") {
  if (require("testthat") &&
    require("parameters") &&
    require("betareg")) {
    data("GasolineYield")
    data("FoodExpenditure")

    m1 <- betareg(yield ~ batch + temp, data = GasolineYield)
    m2 <- betareg(I(food / income) ~ income + persons, data = FoodExpenditure)

    test_that("ci", {
      expect_equal(
        ci(m1)$CI_low,
        c(
          -6.51692, 1.52932, 1.09151, 1.34475, 0.85909, 0.93085, 0.83233,
          0.32981, 0.28241, 0.15335, 0.01016, 224.63213
        ),
        tolerance = 1e-4
      )
      expect_equal(
        ci(m2)$CI_low,
        c(-1.06129, -0.01825, 0.0492, 19.77403),
        tolerance = 1e-4
      )
    })

    test_that("se", {
      expect_equal(
        standard_error(m1)$SE,
        c(
          0.18232, 0.10123, 0.1179, 0.1161, 0.10236, 0.10352, 0.10604,
          0.10913, 0.10893, 0.11859, 0.00041, 110.02562
        ),
        tolerance = 1e-4
      )
      expect_equal(
        standard_error(m2)$SE,
        c(0.22385, 0.00304, 0.03534, 8.0796),
        tolerance = 1e-4
      )
    })

    test_that("p_value", {
      expect_equal(
        p_value(m1)$p,
        c(0, 0, 0, 0, 0, 0, 0, 0, 1e-05, 0.00114, 0, 6e-05),
        tolerance = 1e-4
      )
      expect_equal(
        p_value(m2)$p,
        c(0.00542, 5e-05, 8e-04, 1e-05),
        tolerance = 1e-4
      )
    })

    test_that("model_parameters", {
      expect_equal(
        model_parameters(m1)$Coefficient,
        c(-6.15957, 1.72773, 1.3226, 1.57231, 1.05971, 1.13375, 1.04016, 0.54369, 0.4959, 0.38579, 0.01097),
        tolerance = 1e-4
      )
      expect_equal(
        model_parameters(m1, component = "all")$Coefficient,
        c(-6.15957, 1.72773, 1.3226, 1.57231, 1.05971, 1.13375, 1.04016, 0.54369, 0.4959, 0.38579, 0.01097, 440.27838856),
        tolerance = 1e-4
      )
      expect_equal(model_parameters(m2)$Coefficient, c(-0.62255, -0.0123, 0.11846), tolerance = 1e-4)
      expect_equal(model_parameters(m2, component = "all")$Coefficient, c(-0.62255, -0.0123, 0.11846, 35.60975033), tolerance = 1e-4)
    })
  }
}