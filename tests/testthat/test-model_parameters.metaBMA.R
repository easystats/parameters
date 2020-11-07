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
    expect_equal(params$Parameter, c(
      "Goldstein, Cialdini, & Griskevicius (2008), Exp. 1", "Goldstein, Cialdini, & Griskevicius  (2008), Exp. 2",
      "Schultz, Khazian, & Zaleski (2008), Exp. 2", "Schultz, Khazian, & Zaleski (2008), Exp. 3",
      "Mair & Bergin-Seers (2010), Exp. 1", "Bohner & Schluter (2014), Exp. 1",
      "Bohner & Schluter (2014), Exp. 2", "Overall", "tau"
    ))
    expect_equal(params$Coefficient, c(0.3806, 0.30494, 0.20554, 0.25084, 0.28768, -0.12154, -1.45792, 0.19502, 0.13318), tolerance = 1e-3)
    expect_equal(params$CI_low, c(-0.00686, 0.03816, -0.16998, -0.0825, -1.32685, -0.60772, -2.94785, 0.00542, 0.02117), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c(
        "Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Weight",
        "BF", "Rhat", "ESS", "Component"
      )
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
    expect_equal(params$Parameter, c(
      "Goldstein, Cialdini, & Griskevicius (2008), Exp. 1", "Goldstein, Cialdini, & Griskevicius  (2008), Exp. 2",
      "Schultz, Khazian, & Zaleski (2008), Exp. 2", "Schultz, Khazian, & Zaleski (2008), Exp. 3",
      "Mair & Bergin-Seers (2010), Exp. 1", "Bohner & Schluter (2014), Exp. 1",
      "Bohner & Schluter (2014), Exp. 2", "Overall"
    ))
    expect_equal(params$Coefficient, c(0.3806, 0.30494, 0.20554, 0.25084, 0.28768, -0.12154, -1.45792, 0.21167), tolerance = 1e-3)
    expect_equal(params$CI_low, c(-0.00686, 0.03816, -0.16998, -0.0825, -1.32685, -0.60772, -2.94785, 0.06032), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c(
        "Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Weight",
        "BF", "Rhat", "ESS", "Component"
      )
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
      expect_equal(params$Parameter, c(
        "Goldstein, Cialdini, & Griskevicius (2008), Exp. 1", "Goldstein, Cialdini, & Griskevicius  (2008), Exp. 2",
        "Schultz, Khazian, & Zaleski (2008), Exp. 2", "Schultz, Khazian, & Zaleski (2008), Exp. 3",
        "Mair & Bergin-Seers (2010), Exp. 1", "Bohner & Schluter (2014), Exp. 1",
        "Bohner & Schluter (2014), Exp. 2", "Overall", "tau"
      ))
      expect_equal(params$Coefficient, c(0.3806, 0.30494, 0.20554, 0.25084, 0.28768, -0.12154, -1.45792, 0.19502, 0.13318), tolerance = 1e-3)
      expect_equal(params$CI_low, c(-0.00686, 0.03816, -0.16998, -0.0825, -1.32685, -0.60772, -2.94785, -0.05468, 0.02117), tolerance = 1e-3)
      expect_equal(
        colnames(params),
        c(
          "Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Weight",
          "BF", "Rhat", "ESS", "Component"
        )
      )
    })
  }
}
