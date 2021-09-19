if (requiet("testthat") &&
  requiet("parameters") &&
  requiet("AER")) {
  data("Affairs", package = "AER")
  m1 <- AER::tobit(
    affairs ~ age + yearsmarried + religiousness + occupation + rating,
    data = Affairs
  )

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(2.80106, -0.33435, 0.29049, -2.47756, -0.17261, -3.0843),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(2.74145, 0.07909, 0.13452, 0.40375, 0.25442, 0.40783),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0.00287, 0.02337, 4e-05, 3e-05, 0.20001, 0),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(8.1742, -0.17933, 0.55414, -1.68622, 0.32605, -2.28497),
      tolerance = 1e-4
    )
  })
}
