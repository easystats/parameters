if (require("testthat") && require("parameters")) {
  test_that("model_parameters.htest", {
    params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "pearson"))
    testthat::expect_equal(params$r, -0.852, tolerance = 0.05)

    testthat::expect_warning(params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "spearman")))
    testthat::expect_equal(params$rho, -0.9108, tolerance = 0.05)

    testthat::expect_warning(params <- model_parameters(cor.test(mtcars$mpg, mtcars$cyl, method = "kendall")))
    testthat::expect_equal(params$tau, -0.795, tolerance = 0.05)

    params <- model_parameters(t.test(iris$Sepal.Width, iris$Sepal.Length))
    testthat::expect_equal(params$Difference, -2.786, tolerance = 0.05)

    params <- model_parameters(t.test(mtcars$mpg ~ mtcars$vs))
    testthat::expect_equal(params$Difference, 7.940, tolerance = 0.05)

    params <- model_parameters(t.test(iris$Sepal.Width, mu = 1))
    testthat::expect_equal(params$Difference, 2.0573, tolerance = 0.05)
  })

  if (require("effectsize")) {
    data(mtcars)
    mp <- model_parameters(stats::chisq.test(table(mtcars$am)), cramers_v = "raw", phi = "raw", ci = 0.95)
    test_that("model_parameters-chisq-test raw", {
      expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
      expect_equal(mp$phi, 0.1875, tolerance = 1e-3)
      expect_equal(colnames(mp), c("Chi2", "df", "Cramers_v", "Cramers_CI_low", "Cramers_CI_high", "phi", "phi_CI_low", "phi_CI_high", "p"))
    })

    mp <- model_parameters(stats::chisq.test(table(mtcars$am)))
    test_that("model_parameters-chisq-test NULL", {
      expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
      expect_equal(colnames(mp), c("Chi2", "df", "p"))
    })

    mp <- model_parameters(stats::chisq.test(table(mtcars$am)), phi = "adjusted", ci = 0.95)
    test_that("model_parameters-chisq-test adjusted", {
      expect_equal(mp$Chi2, 1.125, tolerance = 1e-3)
      expect_equal(mp$phi_adjusted, 0.0538348, tolerance = 1e-3)
      expect_equal(colnames(mp), c("Chi2", "df", "phi_adjusted", "phi_CI_low", "phi_CI_high", "p"))
    })
  }
}
