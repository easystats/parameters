if (require("testthat") && require("parameters")) {
  test_that("model_parameters.htest", {
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "pearson"))
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
      expect_equal(colnames(mp), c("Chi2", "df", "Cramers_v", "Cramers_CI_low", "Cramers_CI_high", "phi", "phi_CI_low", "phi_CI_high", "p", "Method"))
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
      expect_equal(colnames(mp), c("Chi2", "df", "phi_adjusted", "phi_CI_low", "phi_CI_high", "p", "Method"))
    })

    set.seed(123)
    params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length), standardized_d = TRUE, hedges_g = TRUE)
    test_that("model_parameters-t-test standardized d and Hedge's g", {
      expect_equal(
        as.data.frame(params),
        structure(
          list(
            Parameter1 = "iris$Sepal.Width",
            Parameter2 = "iris$Sepal.Length",
            Mean_Parameter1 = 3.05733333333333,
            Mean_Parameter2 = 5.84333333333333,
            Difference = -2.786,
            t = -36.4632839344491,
            df_error = 225.678317701878,
            CI_low = -2.93655968048089,
            CI_high = -2.63544031951911,
            Cohens_d = -4.21041735901839,
            d_CI_low = -4.65530594003237,
            d_CI_high = -3.75972614970688,
            Hedges_g = -4.19981177373119,
            g_CI_low = -4.64357972859652,
            g_CI_high = -3.75025580676051,
            p = 1.45954250758609e-96,
            Method = "Welch Two Sample t-test"
          ),
          row.names = c(
            NA,
            -1L
          ),
          class = "data.frame",
          title = "Welch Two Sample t-test",
          model_class = "htest",
          digits = 2,
          ci_digits = 2,
          p_digits = 3,
          ci = 0.95,
          ci_test = 0.95
        ),
        tolerance = 0.05
      )
    })

    mp <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs), standardized_d = TRUE, verbose = FALSE)
    test_that("model_parameters-t-test standardized d", {
      expect_equal(mp$Cohens_d, -1.696032, tolerance = 1e-3)
      expect_equal(
        colnames(mp),
        c(
          "Parameter", "Group", "Mean_Group1", "Mean_Group2", "Difference",
          "t", "df_error", "CI_low", "CI_high", "Cohens_d", "d_CI_low", "d_CI_high",
          "p", "Method"
        )
      )
    })
  }
}
