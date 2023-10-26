test_that("n_factors, default", {
  skip_if_not_installed("nFactors")
  skip_if_not_installed("psych")
  set.seed(333)
  x <- n_factors(mtcars[, 1:4])
  expect_identical(ncol(x), 3L)
})

test_that("n_factors, EGAnet", {
  skip_on_cran()
  skip_if_not_installed("EGAnet")
  set.seed(333)
  x <- n_factors(mtcars, package = "EGAnet")
  expect_identical(ncol(x), 3L)
  expect_identical(
    print(capture.output(x)),
    c(
      "# Method Agreement Procedure:",
      "",
      "The choice of 3 dimensions is supported by 2 (100.00%) methods out of 2 (EGA (glasso), EGA (TMFG))."
    )
  )
})

test_that("n_factors, EGAnet does not fail", {
  skip_on_cran()
  skip_if_not_installed("EGAnet")
  set.seed(333)
  x <- n_factors(mtcars[, 1:4], package = "EGAnet")
  expect_identical(ncol(x), 3L)
  expect_identical(nrow(x), 1L)
  expect_identical(
    print(capture.output(x)),
    c(
      "# Method Agreement Procedure:",
      "",
      "The choice of 1 dimensions is supported by 1 (100.00%) methods out of 1 (EGA (glasso))."
    )
  )
})

test_that("n_factors, oblimin rotation", {
  skip_if_not_installed("nFactors")
  skip_if_not_installed("psych")
  skip_if_not_installed("GPArotation")
  set.seed(333)
  x <- n_factors(mtcars[, 1:4], type = "PCA", rotation = "oblimin")
  expect_identical(ncol(x), 3L)
  expect_identical(
    print(capture.output(x)),
    c(
      "# Method Agreement Procedure:",
      "",
      "The choice of 1 dimensions is supported by 11 (84.62%) methods out of 13 (Bartlett, Anderson, Lawley, Optimal coordinates, Acceleration factor, Parallel analysis, Kaiser criterion, Scree (SE), Scree (R2), VSS complexity 1, Velicer's MAP)." # nolint
    )
  )
})

test_that("n_factors, no rotation, psych only", {
  skip_if_not_installed("nFactors")
  skip_if_not_installed("psych")
  set.seed(333)
  x <- n_factors(mtcars[, 1:4], rotation = "none", package = "psych")
  expect_identical(ncol(x), 3L)
  expect_identical(
    print(capture.output(x)),
    c(
      "# Method Agreement Procedure:",
      "",
      "The choice of 1 dimensions is supported by 3 (60.00%) methods out of 5 (Velicer's MAP, BIC, BIC (adjusted))."
    )
  )
})
