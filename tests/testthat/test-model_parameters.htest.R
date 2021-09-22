if (requiet("testthat") && requiet("parameters") && requiet("effectsize") && utils::packageVersion("effectsize") > "0.4.5") {
  test_that("model_parameters.htest", {
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "pearson"))
    expect_equal(
      colnames(params),
      c(
        "Parameter1", "Parameter2", "r", "CI", "CI_low", "CI_high",
        "t", "df_error", "p", "Method", "Alternative"
      )
    )
    expect_equal(params$r, -0.852, tolerance = 0.05)

    expect_warning(params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "spearman")))
    expect_equal(params$rho, -0.9108, tolerance = 0.05)

    expect_warning(params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "kendall")))
    expect_equal(params$tau, -0.795, tolerance = 0.05)

    params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length))
    expect_equal(params$Difference, -2.786, tolerance = 0.05)

    params <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs))
    expect_equal(params$Difference, -7.940, tolerance = 0.05)

    params <- model_parameters(t.test(iris$Sepal.Width, mu = 1))
    expect_equal(params$Difference, 2.0573, tolerance = 0.05)
  })

  test_that("model_parameters.htest-2", {
    x <- c(A = 20, B = 15, C = 25)
    mp <- model_parameters(chisq.test(x))
    expect_equal(colnames(mp), c("Chi2", "df", "p", "Method"))
  })

  data(mtcars)
  mp <- model_parameters(stats::chisq.test(table(mtcars$am)), cramers_v = "raw", phi = "raw", ci = 0.95)
  test_that("model_parameters-chisq-test raw", {
    expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
    expect_equal(mp$phi, 0.1875, tolerance = 1e-3)
    expect_equal(
      colnames(mp),
      c(
        "Chi2", "df", "Cramers_v", "CI", "Cramers_CI_low", "Cramers_CI_high",
        "phi", "phi_CI_low", "phi_CI_high", "p", "Method"
      )
    )
  })

  mp <- model_parameters(stats::chisq.test(table(mtcars$am)))
  test_that("model_parameters-chisq-test NULL", {
    expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
    expect_equal(colnames(mp), c("Chi2", "df", "p", "Method"))
  })

  mp <- model_parameters(stats::chisq.test(table(mtcars$am)), phi = "adjusted", ci = 0.95)
  test_that("model_parameters-chisq-test adjusted", {
    expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
    expect_equal(mp$phi_adjusted, 0.0538348, tolerance = 1e-3)
    expect_equal(colnames(mp), c("Chi2", "df", "phi_adjusted", "CI", "phi_CI_low", "phi_CI_high", "p", "Method"))
  })

  params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length), standardized_d = TRUE)
  test_that("model_parameters-t-test standardized d", {
    expect_equal(params$Cohens_d, -4.210417, tolerance = 0.05)
    expect_equal(params$d_CI_low, -4.655306, tolerance = 0.05)
    expect_equal(
      colnames(params),
      c(
        "Parameter1", "Parameter2", "Mean_Parameter1", "Mean_Parameter2",
        "Difference", "CI", "CI_low", "CI_high", "t", "df_error", "Cohens_d", "d_CI_low",
        "d_CI_high", "p", "Method", "Alternative"
      )
    )
  })

  mp <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs), standardized_d = TRUE, verbose = FALSE)
  test_that("model_parameters-t-test standardized d", {
    expect_equal(mp$Cohens_d, -1.696032, tolerance = 1e-3)
    expect_equal(
      colnames(mp),
      c(
        "Parameter", "Group", "Mean_Group1", "Mean_Group2", "Difference", "CI",
        "CI_low", "CI_high", "t", "df_error", "Cohens_d", "d_CI_low", "d_CI_high",
        "p", "Method", "Alternative"
      )
    )
  })

  test_that("model_parameters-t-test reports the same unregarding of interface", {
    g1 <- 1:10
    g2 <- 7:20
    df <- data.frame(y = c(g1, g2), x = rep(c(0, 1), c(length(g1), length(g2))))
    compare_only <- c("Difference", "CI", "CI_low", "CI_high", "t", "df_error", "p", "Method")
    default_ttest <- model_parameters(t.test(x = g1, y = g2))[compare_only]
    formula_ttest <- model_parameters(t.test(y ~ x, df))[compare_only]
    expect_equal(default_ttest, formula_ttest)
  })
}
