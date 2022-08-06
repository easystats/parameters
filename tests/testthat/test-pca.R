if (requiet("testthat") && requiet("parameters") && requiet("psych") && requiet("nFactors")) {
  test_that("principal_components", {
    x <- parameters::principal_components(mtcars[, 1:7], rotation = "varimax")

    expect_equal(
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

    expect_equal(
      colnames(x),
      c("Variable", "RC1", "RC2", "Complexity", "Uniqueness", "MSA")
    )

    expect_equal(
      dim(predict(x)),
      c(32, 2)
    )
    expect_equal(
      names(predict(x, names=c("A", "B"))),
      c("A", "B")
    )
    expect_equal(
      nrow(predict(x, newdata = mtcars[1:3, 1:7])),
      3
    )
  })


  test_that("principal_components", {
    x <- parameters::principal_components(mtcars[, 1:7])

    expect_equal(
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

    expect_equal(
      colnames(x),
      c("Variable", "PC1", "PC2", "Complexity")
    )
  })

  test_that("principal_components", {
    x <- model_parameters(principal(mtcars[, 1:7], nfactors = 2))

    expect_equal(
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

    expect_equal(
      colnames(x),
      c("Variable", "RC1", "RC2", "Complexity", "Uniqueness")
    )

    expect_equal(
      dim(suppressWarnings(predict(x))),
      c(32, 2)
    )
    expect_equal(
      dim(suppressWarnings(predict(x, newdata=mtcars[1:3, 1:7]))),
      c(3, 2)
    )
  })


  # predict ----------------------
  # N.B tests will fail if `GPArotation` package is not installed

  data(bfi)
  d <- na.omit(bfi[, 1:25])
  model <- psych::fa(d, nfactors = 5)
  mp <- model_parameters(model, sort = TRUE, threshold = "max")

  test_that("predict model_parameters fa", {
    pr <- suppressWarnings(predict(mp, names = c("Neuroticism", "Conscientiousness", "Extraversion", "Agreeableness", "Opennness")))
    out <- head(pr, 5)
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

    expect_equal(
      nrow(predict(mp, keep_na = FALSE)),
      2436
    )

    expect_equal(
      nrow(predict(mp, newdata=d[1:10, ], keep_na = FALSE)),
      10
    )
  })


  model <- factor_analysis(d, n = 5)

  test_that("predict factor_analysis", {
    expect_equal(
      nrow(predict(model, keep_na = FALSE)),
      2436
    )

    expect_equal(
      nrow(predict(mp, newdata=d[1:10, ], keep_na = FALSE)),
      10
    )

    expect_equal(
      names(predict(mp, names=c("A", "B", "C", "D", "E"), keep_na = FALSE)),
      c('A', 'B', 'C', 'D', 'E')
    )
  })
}
