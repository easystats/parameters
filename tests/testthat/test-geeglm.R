if (require("testthat") &&
  require("parameters") &&
  require("geepack")) {

  data(warpbreaks)
  m1 <-
    geeglm(
      breaks ~ tension,
      id = wool,
      data = warpbreaks,
      family = poisson,
      corstr = "ar1"
    )

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(3.28294, -0.76741, -0.64708),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.15931, 0.22554, 0.06598),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0.14913, 0),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(3.59517, -0.32536, -0.51776),
      tolerance = 1e-4
    )
  })
}
