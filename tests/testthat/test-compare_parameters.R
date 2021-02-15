if (require("testthat") && require("parameters")) {
  data(iris)
  m1 <- lm(Sepal.Length ~ Species, data = iris)
  m2 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  m3 <- glm(counts ~ outcome + treatment, family = poisson())

  x <- compare_parameters(m1, m2, m3)
  test_that("compare_parameters, default", {
    expect_equal(
      colnames(x),
      c("Parameter", "Component", "Coefficient.m1", "SE.m1", "CI.m1",
        "CI_low.m1", "CI_high.m1", "t.m1", "df_error.m1", "p.m1", "Coefficient.m2",
        "SE.m2", "CI.m2", "CI_low.m2", "CI_high.m2", "t.m2", "df_error.m2",
        "p.m2", "Log-Mean.m3", "SE.m3", "CI.m3", "CI_low.m3", "CI_high.m3",
        "z.m3", "df_error.m3", "p.m3")
    )
    out <- capture.output(x)
    expect_equal(length(out), 16)
    expect_equal(
      out[3],
      "(Intercept)                         | 5.01 (4.86, 5.15) |  4.21 ( 3.41,  5.02) |     3.04 ( 2.70,  3.37)"
    )
    out <- format(x, style = "ci")
    expect_equal(colnames(out), c("Parameter", "m1", "m2", "m3"))
    expect_equal(
      out$Parameter,
      c("(Intercept)", "Species (versicolor)", "Species (virginica)",
        "Species (versicolor)", "Species (virginica)", "Petal.Length",
        "Species (versicolor) * Petal.Length", "Species (virginica) * Petal.Length",
        "outcome (2)", "outcome (3)", "treatment (2)", "treatment (3)",
        NA, "Observations")
    )
    expect_equal(
      out$m3,
      c("3.04 2.70, 3.37", "", "", "", "", "", "", "", "-0.45 -0.86, -0.06",
        "-0.29 -0.68, 0.08", "1.34e-15 -0.39, 0.39", "1.42e-15 -0.39, 0.39",
        NA, "9")
    )
  })


  x <- compare_parameters(m1, m2, m3, style = "se_p2")
  test_that("compare_parameters, se_p2", {
    expect_equal(
      colnames(x),
      c("Parameter", "Component", "Coefficient.m1", "SE.m1", "CI.m1",
        "CI_low.m1", "CI_high.m1", "t.m1", "df_error.m1", "p.m1", "Coefficient.m2",
        "SE.m2", "CI.m2", "CI_low.m2", "CI_high.m2", "t.m2", "df_error.m2",
        "p.m2", "Log-Mean.m3", "SE.m3", "CI.m3", "CI_low.m3", "CI_high.m3",
        "z.m3", "df_error.m3", "p.m3")
    )
    out <- capture.output(x)
    expect_equal(length(out), 16)
    expect_equal(
      out[3],
      "(Intercept)                         |      5.01 (0.07) | < .001 |      4.21 (0.41) | < .001 |     3.04 (0.17) | < .001"
    )
    out <- format(x, style = "se_p2")
    expect_equal(
      colnames(out),
      c("Parameter", "Coefficient (m1)", "p (m1)", "Coefficient (m2)",
        "p (m2)", "Log-Mean (m3)", "p (m3)")
    )
    expect_equal(
      out$Parameter,
      c("(Intercept)", "Species (versicolor)", "Species (virginica)",
        "Species (versicolor)", "Species (virginica)", "Petal.Length",
        "Species (versicolor) * Petal.Length", "Species (virginica) * Petal.Length",
        "outcome (2)", "outcome (3)", "treatment (2)", "treatment (3)",
        NA, "Observations")
    )
    expect_equal(
      out$`Log-Mean (m3)`,
      c("3.04 (0.17)", "", "", "", "", "", "", "", "-0.45 (0.20)",
        "-0.29 (0.19)", "1.34e-15 (0.20)", "1.42e-15 (0.20)", NA, "9")
    )
    expect_equal(
      out$`p (m3)`,
      c("< .001", "      ", "      ", "      ", "      ", "      ",
        "      ", "      ", "0.025 ", "0.128 ", "> .999", "> .999", NA, NA)
    )
  })
}
