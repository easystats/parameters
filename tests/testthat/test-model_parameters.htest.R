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

}