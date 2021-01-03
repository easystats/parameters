if (require("testthat") && require("parameters")) {
  test_that("model_parameters.htest", {
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "pearson"))
    expect_equal(params$r, -0.852, tolerance = 0.05)

    expect_warning(params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "spearman")))
    expect_equal(params$rho, -0.9108, tolerance = 0.05)

    expect_warning(params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "kendall")))
    expect_equal(params$tau, -0.795, tolerance = 0.05)

    params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length))
    expect_equal(params$Mean_Difference, -2.786, tolerance = 0.05)

    params <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs))
    expect_equal(params$Mean_Difference, 7.940, tolerance = 0.05)

    params <- model_parameters(t.test(iris$Sepal.Width, mu = 1))
    expect_equal(params$Mean_Difference, 2.0573, tolerance = 0.05)
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
    params <- model_parameters(t.test(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2]),
      standardized_d = TRUE,
      hedges_g = TRUE
    )
    test_that("model_parameters-t-test standardized d and Hedge's g", {
      expect_equal(
        as.data.frame(params),
        structure(
          list(
            Parameter1 = "sleep$extra[sleep$group == 1]",
            Parameter2 = "sleep$extra[sleep$group == 2]",
            Mean_Parameter1 = 0.75,
            Mean_Parameter2 = 2.33,
            Mean_Difference = -1.58,
            Difference_CI_low = -3.36548323071171,
            Difference_CI_high = 0.20548323071171,
            t = -1.86081346748685,
            df_error = 17.7764735161785,
            Cohens_d = -0.83218108134954,
            d_CI_low = -1.73918888274057,
            d_CI_high = 0.0960614913209062,
            Hedges_g = -0.797018500447446,
            g_CI_low = -1.66570202854027,
            g_CI_high = 0.0920025550679102,
            p = 0.0793941401873582,
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
          "Parameter", "Group", "Mean_Group1", "Mean_Group2", "Mean_Difference", "Difference_CI_low", "Difference_CI_high",
          "t", "df_error", "Cohens_d", "d_CI_low", "d_CI_high", "p", "Method"
        )
      )
    })
  }
}
