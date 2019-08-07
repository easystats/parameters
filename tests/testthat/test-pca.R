context("principal_components")

test_that("principal_components", {
  x <- parameters::principal_components(mtcars[, 1:7], rotation = "varimax")

  testthat::expect_equal(
    x$RC1,
    c(
      -0.836114674884308,
      0.766808147590597,
      0.85441780762136,
      0.548502661888057,
      -0.889046093964722,
      0.931879020871552,
      -0.030485507571411
    ),
    tolerance = 0.01
  )

  testthat::expect_equal(
    colnames(x),
    c("Variable", "RC1", "RC2", "Complexity", "Uniqueness")
  )
})


test_that("principal_components", {
  x <- parameters::principal_components(mtcars[, 1:7])

  testthat::expect_equal(
    x$PC1,
    c(
      -0.930866058535747,
      0.9578708009312,
      0.952846253483008,
      0.874493647245971,
      -0.746868056938478,
      0.882509152331738,
      -0.541093678419456
    ),
    tolerance = 0.01
  )

  testthat::expect_equal(
    colnames(x),
    c("Variable", "PC1", "PC2", "Complexity")
  )
})
