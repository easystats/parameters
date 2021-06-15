.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (require("testthat") &&
  require("parameters") &&
  require("lme4") &&
  require("GLMMadaptive")) {
  data("fish")
  data("cbpp")

  m1 <- mixed_model(
    count ~ child + camper,
    random = ~ 1 | persons,
    zi_fixed = ~ child + livebait,
    data = fish,
    family = zi.poisson()
  )
  m2 <- mixed_model(
    cbind(incidence, size - incidence) ~ period,
    random = ~ 1 | herd,
    data = cbpp,
    family = binomial
  )

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(0.08708, -1.35715, 0.58599, -0.99993, 0.75543, -2.1166),
      tolerance = 1e-3
    )
    expect_equal(
      ci(m1, component = "cond")$CI_low,
      c(0.08708, -1.35715, 0.58599),
      tolerance = 1e-3
    )
    expect_equal(
      ci(m1, component = "zi")$CI_low,
      c(-0.99993, 0.75543, -2.1166),
      tolerance = 1e-3
    )

    expect_equal(
      ci(m2)$CI_low,
      c(-1.8572, -1.59265, -1.76827, -2.41754),
      tolerance = 1e-3
    )
    expect_equal(
      ci(m2, component = "cond")$CI_low,
      c(-1.8572, -1.59265, -1.76827, -2.41754),
      tolerance = 1e-3
    )

    expect_null(ci(m2, component = "zi"))
  })



  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.54002, 0.09485, 0.09356, 0.46812, 0.29416, 0.50763),
      tolerance = 1e-3
    )
    expect_equal(
      standard_error(m1, component = "cond")$SE,
      c(0.54002, 0.09485, 0.09356),
      tolerance = 1e-3
    )
    expect_equal(
      standard_error(m1, component = "zi")$SE,
      c(0.46812, 0.29416, 0.50763),
      tolerance = 1e-3
    )

    expect_equal(
      standard_error(m2)$SE,
      c(0.23354, 0.30678, 0.32678, 0.42761),
      tolerance = 1e-3
    )
    expect_equal(
      standard_error(m2, component = "cond")$SE,
      c(0.23354, 0.30678, 0.32678, 0.42761),
      tolerance = 1e-3
    )

    expect_null(standard_error(m2, component = "zi"))
  })


  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0.0339, 0, 0, 0.86023, 1e-05, 0.02713),
      tolerance = 1e-3
    )
    expect_equal(
      p_value(m1, component = "cond")$p,
      c(0.0339, 0, 0),
      tolerance = 1e-3
    )
    expect_equal(
      p_value(m1, component = "zi")$p,
      c(0.86023, 1e-05, 0.02713),
      tolerance = 1e-3
    )

    expect_equal(
      p_value(m2)$p,
      c(0, 0.00123, 0.00056, 0.00022),
      tolerance = 1e-3
    )
    expect_equal(
      p_value(m2, component = "cond")$p,
      c(0, 0.00123, 0.00056, 0.00022),
      tolerance = 1e-3
    )

    expect_null(p_value(m2, component = "zi"))
  })


  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1, effects = "fixed")$Coefficient,
      c(1.14549, -1.17125, 0.76937, -0.08243, 1.33197, -1.12165),
      tolerance = 1e-3
    )
    expect_equal(
      model_parameters(m2, effects = "fixed")$Coefficient,
      c(-1.39946, -0.99138, -1.1278, -1.57945),
      tolerance = 1e-3
    )
  })

  if (.runThisTest && require("glmmTMB")) {
    data("Salamanders")
    model <- mixed_model(
      count ~ spp + mined,
      random = ~ DOY | site,
      zi_fixed = ~ spp + mined,
      zi_random = ~ DOP | site,
      family = zi.negative.binomial(),
      data = Salamanders,
      control = list(nAGQ = 1)
    )

    test_that("model_parameters.mixed-ran_pars", {
      params <- model_parameters(model, effects = "random")
      expect_equal(c(nrow(params), ncol(params)), c(8, 9))
      expect_equal(
        colnames(params),
        c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "Effects", "Group", "Component")
      )
      expect_equal(
        params$Parameter,
        c(
          "SD (Intercept)", "SD (DOY)", "Cor (Intercept~site)", "SD (Observations)",
          "SD (Intercept)", "SD (DOP)", "Cor (Intercept~site)", "SD (Observations)"
        )
      )
      expect_equal(
        params$Component,
        c(
          "conditional", "conditional", "conditional", "conditional",
          "zero_inflated", "zero_inflated", "zero_inflated", "zero_inflated"
        )
      )
      expect_equal(
        params$Coefficient,
        c(0.56552, 0.29951, 0.06307, 1.61936, 1.02233, 0.38209, -0.17162, 1.61936),
        tolerance = 1e-2
      )
    })
  }
}
