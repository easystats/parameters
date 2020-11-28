if (require("testthat") && require("parameters") && require("psych")) {
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
      c("Variable", "RC1", "RC2", "Complexity", "Uniqueness", "MSA")
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


  # predict ----------------------

  data(bfi)
  d <- na.omit(bfi[, 1:25])
  model <- psych::fa(data, nfactors = 5)
  mp <- model_parameters(model, sort = TRUE, threshold = "max")

  test_that("predict model_parameters fa", {
    out <- head(predict(mp, names = c("Neuroticism", "Conscientiousness", "Extraversion", "Agreeableness", "Opennness")), 5)
    expect_equal(
      out$Neuroticism,
      c(-0.22242, 0.1618, 0.61907, -0.11692, -0.17372),
      tolerance = 0.01
    )
    expect_equal(
      out$Opennness,
      c(-1.6092, -0.17222, 0.23341, -1.06152, -0.66086),
      tolerance = 0.01
    )
  })
}
