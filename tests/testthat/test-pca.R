skip_if_not_installed("psych")
skip_if_not_installed("nFactors")
skip_if_not_installed("GPArotation")

test_that("principal_components", {
  x <- principal_components(mtcars[, 1:7], rotation = "varimax")

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

  expect_named(x, c("Variable", "RC1", "RC2", "Complexity", "Uniqueness", "MSA"))

  expect_identical(dim(predict(x)), c(32L, 2L))
  expect_named(predict(x, names = c("A", "B")), c("A", "B"))
  expect_identical(nrow(predict(x, newdata = mtcars[1:3, 1:7])), 3L)
})


test_that("principal_components, n", {
  data(iris)
  x <- principal_components(iris[1:4], n = 2)
  expect_named(x, c("Variable", "PC1", "PC2", "Complexity"))

  x <- principal_components(iris[1:4], n = 1)
  expect_named(x, c("Variable", "PC1", "Complexity"))
})


test_that("principal_components", {
  x <- principal_components(mtcars[, 1:7])

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

  expect_named(x, c("Variable", "PC1", "PC2", "Complexity"))
  expect_identical(dim(predict(x)), c(32L, 2L))
})


# print ----

test_that("print model_parameters pca", {
  data(mtcars)
  expect_snapshot(print(principal_components(mtcars[, 1:4], n = "auto")))
  expect_snapshot(print(
    principal_components(mtcars[, 1:4], n = "auto"),
    labels = c(
      "Miles/(US) gallon",
      "Number of cylinders",
      "Displacement (cu.in.)",
      "Gross horsepower"
    )
  ))
})


# predict ----------------------
# N.B tests will fail if `GPArotation` package is not installed

test_that("predict model_parameters fa", {
  d <- na.omit(psych::bfi[, 1:25])
  model <- psych::fa(d, nfactors = 5)
  mp <- model_parameters(model, sort = TRUE, threshold = "max")
  pr <- suppressMessages(
    predict(mp, names = c("Neuroticism", "Conscientiousness", "Extraversion", "Agreeableness", "Opennness"))
  )
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
  expect_identical(nrow(predict(mp, keep_na = FALSE)), 2436L)
  expect_identical(nrow(predict(mp, newdata = d[1:10, ], keep_na = FALSE)), 10L)
  expect_named(
    predict(mp, names = c("A", "B", "C", "D", "E"), keep_na = FALSE),
    c("A", "B", "C", "D", "E")
  )
  model <- factor_analysis(d, n = 5, rotation = "none")
  expect_identical(nrow(predict(model, keep_na = FALSE)), 2436L)
})

unloadNamespace("GPArotation")
