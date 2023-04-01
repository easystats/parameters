options(parameters_exponentiate = FALSE)

if (requiet("mclogit") && packageVersion("insight") > "0.19.1") {
  data(Transport)
  m1 <- mclogit(
    cbind(resp, suburb) ~ distance + cost,
    data = Transport
  )

  data(housing, package = "MASS")
  m2 <- mblogit(Sat ~ Infl + Type + Cont,
    weights = Freq,
    data = housing
  )

  test_that("model_parameters.mclogit", {
    params <- model_parameters(m1)
    expect_message(out <- capture.output(print(params)))
    expect_identical(
      out,
      c(
        "Parameter | Log-Odds |   SE |         95% CI |      z |      p",
        "--------------------------------------------------------------",
        "distance  |    -1.44 | 0.05 | [-1.54, -1.34] | -27.07 | < .001",
        "cost      |    -0.98 | 0.04 | [-1.06, -0.90] | -24.52 | < .001"
      )
    )
  })
  test_that("model_parameters.mblogit", {
    params <- model_parameters(m2)
    expect_message(out <- capture.output(print(params)))
    expect_identical(
      out,
      c(
        "# Response level: medium",
        "",
        "Parameter     | Log-Odds |   SE |         95% CI |     z |      p",
        "-----------------------------------------------------------------",
        "(Intercept)   |    -0.42 | 0.17 | [-0.76, -0.08] | -2.42 | 0.015 ",
        "InflMedium    |     0.45 | 0.14 | [ 0.17,  0.72] |  3.15 | 0.002 ",
        "InflHigh      |     0.66 | 0.19 | [ 0.30,  1.03] |  3.57 | < .001",
        "TypeApartment |    -0.44 | 0.17 | [-0.77, -0.10] | -2.53 | 0.012 ",
        "TypeAtrium    |     0.13 | 0.22 | [-0.31,  0.57] |  0.59 | 0.556 ",
        "TypeTerrace   |    -0.67 | 0.21 | [-1.07, -0.26] | -3.23 | 0.001 ",
        "ContHigh      |     0.36 | 0.13 | [ 0.10,  0.62] |  2.73 | 0.006 ",
        "",
        "# Response level: high",
        "",
        "Parameter     | Log-Odds |   SE |         95% CI |     z |      p",
        "-----------------------------------------------------------------",
        "(Intercept)   |    -0.14 | 0.16 | [-0.45,  0.17] | -0.87 | 0.384 ",
        "InflMedium    |     0.73 | 0.14 | [ 0.47,  1.00] |  5.37 | < .001",
        "InflHigh      |     1.61 | 0.17 | [ 1.29,  1.94] |  9.65 | < .001",
        "TypeApartment |    -0.74 | 0.16 | [-1.04, -0.43] | -4.74 | < .001",
        "TypeAtrium    |    -0.41 | 0.21 | [-0.82,  0.01] | -1.93 | 0.054 ",
        "TypeTerrace   |    -1.41 | 0.20 | [-1.80, -1.02] | -7.06 | < .001",
        "ContHigh      |     0.48 | 0.12 | [ 0.24,  0.73] |  3.88 | < .001"
      )
    )
  })
}

options(parameters_exponentiate = NULL)
