if (require("testthat") &&
  require("parameters") &&
  require("pscl")) {
  data("bioChemists")
  m1 <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(0.42844, -0.34446, 0.00734, -0.26277, 0.01717, -1.77978, -0.37558, -0.51411),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.06797, 0.05868, 0.06593, 0.04874, 0.00212, 0.43378, 0.21509, 0.1352),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 9e-05, 0.03833, 6e-04, 0, 0.03211, 0.83068, 0.06539),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(0.56167, -0.22945, 0.13656, -0.16725, 0.02132, -0.92959, 0.04599, -0.24912),
      tolerance = 1e-4
    )
  })


  ## TODO activate once insight update on CRAN

  # m2 <- zeroinfl(formula = art ~ . | 1, data = bioChemists, dist = "negbin")
  # test_that("model_parameters", {
  #   expect_equal(
  #     model_parameters(m2)$Coefficient,
  #     c(0.25615, -0.21642, 0.15049, -0.17642, 0.01527, 0.02908, -11.95447),
  #     tolerance = 1e-4
  #   )
  #   expect_equal(
  #     model_parameters(m2)$Coefficient,
  #     c("conditional", "conditional", "conditional", "conditional",
  #       "conditional", "conditional", "zero_inflated")
  #   )
  # })
}
