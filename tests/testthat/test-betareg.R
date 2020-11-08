if (Sys.getenv("USER") != "travis") {
  if (require("testthat") && require("parameters") && require("betareg")) {
    data("GasolineYield")
    data("FoodExpenditure")

    set.seed(123)
    m1 <- betareg(yield ~ batch + temp, data = GasolineYield)

    set.seed(123)
    m2 <- betareg(I(food / income) ~ income + persons, data = FoodExpenditure)

    test_that("ci", {
      expect_equal(
        ci(m1)$CI_low,
        as.vector(confint(m1)[, 1]),
        tolerance = 1e-4
      )
      expect_equal(
        ci(m2)$CI_low,
        as.vector(confint(m2)[, 1]),
        tolerance = 1e-4
      )
    })

    test_that("se", {
      s <- summary(m1)
      expect_equal(
        standard_error(m1)$SE,
        as.vector(c(s$coefficients$mean[, 2], s$coefficients$precision[, 2])),
        tolerance = 1e-4
      )
      s <- summary(m2)
      expect_equal(
        standard_error(m2)$SE,
        as.vector(c(s$coefficients$mean[, 2], s$coefficients$precision[, 2])),
        tolerance = 1e-4
      )
    })

    test_that("p_value", {
      expect_equal(
        p_value(m1)$p,
        c(
          3.44252055464711e-250, 2.59222187511485e-65, 3.33610354513962e-29,
          8.81149816157614e-42, 4.06285456374022e-25, 6.52376139739937e-28,
          1.02503603164473e-22, 6.28732658566544e-07, 5.2973871616935e-06,
          0.00114162806954335, 1.25703117565381e-155, 6.29159627280523e-05
        ),
        tolerance = 1e-4
      )
      expect_equal(
        p_value(m2)$p,
        c(
          0.00541832640147535, 5.08781925792733e-05, 0.000802285274571123,
          1.04635071814826e-05
        ),
        tolerance = 1e-4
      )
    })

    test_that("model_parameters", {
      expect_equal(
        model_parameters(m1)$Coefficient,
        as.vector(coef(m1))[1:11],
        tolerance = 1e-4
      )
      expect_equal(
        model_parameters(m1, component = "all")$Coefficient,
        as.vector(coef(m1)),
        tolerance = 1e-4
      )
      expect_equal(model_parameters(m2)$Coefficient, c(-0.62255, -0.0123, 0.11846), tolerance = 1e-4)
      expect_equal(model_parameters(m2, component = "all")$Coefficient, c(-0.62255, -0.0123, 0.11846, 35.60975033), tolerance = 1e-4)
    })
  }
}
