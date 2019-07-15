context("check_factorstructure")

test_that("check_factorstructure", {
  x <- check_factorstructure(mtcars)
  testthat::expect_equal(x$KMO$MSA, 0.826, tol = 0.01)
  testthat::expect_equal(x$sphericity$chisq, 408.011, tol = 0.01)
})
