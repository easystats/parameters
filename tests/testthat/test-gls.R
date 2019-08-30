if (require("testthat") &&
  require("parameters") &&
  require("nlme") &&
  require("lme4")) {

  data(Ovary)
  m1 <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
    data = Ovary,
    correlation = corAR1(form = ~ 1 | Mare)
  )

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(10.9137205623851, -4.03898261140754, -2.26675468048102),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.664643651063474, 0.645047778144975, 0.697538308948056),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(2.6187369542827e-51, 2.28628382225752e-05, 0.198137111907874),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(12.2163981810227, -2.77471219793581, -0.899604717105857),
      tolerance = 1e-4
    )
  })
}
