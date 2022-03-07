if (requiet("testthat") &&
  requiet("parameters") &&
  requiet("pscl")) {
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
    expect_s3_class(p_value(m1, method = "robust"), "data.frame")
    expect_s3_class(p_value(m1, method = "robust", vcov = NULL), "data.frame")
    expect_s3_class(p_value(m1, vcov = NULL), "data.frame")
    expect_s3_class(p_value(m1, vcov = "HC"), "data.frame")
    expect_s3_class(p_value(m1, method = "robust", vcov = "HC"), "data.frame")
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      as.vector(coef(m1)),
      tolerance = 1e-4
    )
  })


  m2 <- zeroinfl(formula = art ~ . | 1, data = bioChemists, dist = "negbin")
  test_that("model_parameters", {
    expect_equal(
      model_parameters(m2)$Coefficient,
      as.vector(coef(m2)),
      tolerance = 1e-4
    )
    expect_equal(
      model_parameters(m2)$Component,
      c(
        "conditional", "conditional", "conditional", "conditional",
        "conditional", "conditional", "zero_inflated"
      )
    )
  })

  m3 <- zeroinfl(art ~ mar + kid5 * fem + ment | kid5 * fem + phd, data = bioChemists)

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m3)$Coefficient,
      as.vector(coef(m3)),
      tolerance = 1e-4
    )
  })

  test_that("parameters_type", {
    expect_equal(
      parameters_type(m3)$Type,
      c(
        "intercept", "factor", "numeric", "factor", "numeric", "interaction",
        "intercept", "numeric", "factor", "numeric", "interaction"
      ),
      tolerance = 1e-4
    )
  })

  test_that("parameters_type", {
    expect_equal(
      parameters_type(m3)$Link,
      c(
        "Mean", "Difference", "Association", "Difference", "Association",
        "Difference", "Mean", "Association", "Difference", "Association",
        "Difference"
      ),
      tolerance = 1e-4
    )
  })

}
