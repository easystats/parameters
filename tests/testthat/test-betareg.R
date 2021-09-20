if (requiet("testthat") && requiet("parameters") && requiet("betareg")) {
  data("GasolineYield")
  data("FoodExpenditure")

  m1 <- betareg(yield ~ batch + temp, data = GasolineYield)
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
      c(0, 0, 0, 0, 0, 0, 0, 0, 1e-05, 0.00114, 0, 6e-05),
      tolerance = 1e-3
    )
    expect_equal(
      p_value(m2)$p,
      c(0.00542, 5e-05, 8e-04, 1e-05),
      tolerance = 1e-3
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
