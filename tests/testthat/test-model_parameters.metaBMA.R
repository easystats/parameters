if (require("testthat") && require("insight") && require("parameters") && require("metaBMA")) {
  data(towels)

  # default
  set.seed(1234)
  m <-
    suppressWarnings(meta_random(
      logOR,
      SE,
      study,
      data = towels,
      ci = 0.95,
      iter = 100,
      logml_iter = 200
    ))

  test_that("model_parameters.meta_random", {
    params <- model_parameters(m)
    expect_equal(params$Parameter, c("Overall", "tau"))
    expect_equal(params$Coefficient, c(0.1950243, 0.1331831), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "BF",
        "Rhat", "ESS")
    )
  })

  set.seed(1234)
  m2 <-
    meta_fixed(
      logOR,
      SE,
      study,
      data = towels,
      ci = 0.95
    )

  test_that("model_parameters.meta_fixed", {
    params <- model_parameters(m2)
    expect_equal(params$Parameter, "Overall")
    expect_equal(params$Coefficient, 0.2116664, tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "BF",
        "Rhat", "ESS")
    )
  })
}
