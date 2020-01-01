context("check_*")

test_that("check_factorstructure", {
  x <- check_factorstructure(mtcars)
  testthat::expect_equal(x$KMO$MSA, 0.826, tol = 0.01)
  testthat::expect_equal(x$sphericity$chisq, 408.011, tol = 0.01)
})

test_that("check_clusterstructure", {
  set.seed(333)
  testthat::expect_equal(check_clusterstructure(iris[, 1:4])$H, 0.187, tol = 0.01)
})
