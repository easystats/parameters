if (require("testthat") && require("parameters")) {
  test_that("model_parameters.htest", {
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "pearson"))
    expect_equal(
      colnames(params),
      c(
        "Parameter1", "Parameter2", "r", "CI", "CI_low", "CI_high",
        "t", "df_error", "p", "Method"
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
    expect_equal(params$Difference, 7.940, tolerance = 0.05)

    params <- model_parameters(t.test(iris$Sepal.Width, mu = 1))
    expect_equal(params$Difference, 2.0573, tolerance = 0.05)
  })

  test_that("model_parameters.htest-2", {
    x <- c(A = 20, B = 15, C = 25)
    mp <- model_parameters(chisq.test(x))
    expect_equal(colnames(mp), c("Chi2", "df", "p", "Method"))
  })

  if (require("effectsize")) {
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
          "d_CI_high", "p", "Method"
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
          "p", "Method"
        )
      )
    })
  }
}

if (require("effectsize")) {
  test_that("model_parameters-rank biserial", {
    x <- c(1.83, 0.50, 1.62, 2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
    y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
    mod_paired <- wilcox.test(x, y, paired = TRUE, alternative = "greater")

    set.seed(123)
    expect_snapshot(model_parameters(mod_paired))

    x <- c(1.15, 0.88, 0.90, 0.74, 1.21)
    mod_one <- wilcox.test(x, mu = 1)

    set.seed(123)
    expect_snapshot(model_parameters(mod_one))

    m <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
    n <- c(1.15, 0.88, 0.90, 0.74, 1.21)
    mod_unpaired <- wilcox.test(m, n, alternative = "g")

    set.seed(123)
    expect_snapshot(model_parameters(mod_unpaired))
  })

  test_that("model_parameters- rank epsilon squared", {
    x <- c(2.9, 3.0, 2.5, 2.6, 3.2)
    y <- c(3.8, 2.7, 4.0, 2.4)
    z <- c(2.8, 3.4, 3.7, 2.2, 2.0)

    x <- c(x, y, z)
    g <- factor(rep(1:3, c(5, 4, 5)),
      labels = c(
        "Normal subjects",
        "Subjects with obstructive airway disease",
        "Subjects with asbestosis"
      )
    )
    mod <- kruskal.test(x, g)

    set.seed(123)
    expect_snapshot(model_parameters(mod))
  })

  if (packageVersion("insight") > "0.13.2") {
    test_that("model_parameters- Kendall's W", {
      wb <- aggregate(warpbreaks$breaks,
        by = list(
          w = warpbreaks$wool,
          t = warpbreaks$tension
        ),
        FUN = mean
      )

      mod <- friedman.test(x ~ w | t, data = wb)

      set.seed(123)
      expect_snapshot(suppressWarnings(model_parameters(mod)))
    })
  }
}
