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
    expect_equal(params$CI_low, c(0.005415821, 0.021171257), tolerance = 1e-3)
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
    expect_equal(params$CI_low, 0.06031935, tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "BF",
        "Rhat", "ESS")
    )
  })

  if (packageVersion("metaBMA") > "0.6.3") {
    set.seed(1234)
    m3 <-
      suppressWarnings(meta_random(
        logOR,
        SE,
        study,
        data = towels,
        ci = 0.99,
        iter = 100,
        logml_iter = 200
      ))

    test_that("model_parameters.meta_random", {
      params <- model_parameters(m3)
      expect_equal(params$Parameter, c("Overall", "tau"))
      expect_equal(params$Coefficient, c(0.1950243, 0.1331831), tolerance = 1e-3)
      expect_equal(params$CI_low, c(-0.05468293, 0.02117126), tolerance = 1e-3)
      expect_equal(
        colnames(params),
        c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "BF",
          "Rhat", "ESS")
      )
    })
  }
}
