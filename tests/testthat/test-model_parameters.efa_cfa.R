test_that("principal_components", {
  set.seed(333)
  x <- principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
  expect_identical(c(ncol(x), nrow(x)), c(8L, 7L))

  x <- suppressMessages(principal_components(
    mtcars[, 1:7],
    n = 2,
    rotation = "oblimin",
    threshold = "max",
    sort = TRUE
  ))
  expect_identical(c(ncol(x), nrow(x)), c(6L, 7L))

  pca <- principal_components(mtcars[, 1:5], n = 2)
  expect_identical(c(ncol(pca), nrow(pca)), c(4L, 5L))
  x <- summary(pca)
  expect_identical(c(ncol(x), nrow(x)), c(3L, 4L))
  x <- model_parameters(pca)
  expect_identical(c(ncol(x), nrow(x)), c(5L, 2L))
  x <- predict(pca)
  expect_identical(c(ncol(x), nrow(x)), c(2L, 32L))
})

test_that("efa-cfa", {
  skip_if_not_installed("psych")
  skip_if_not_installed("lavaan")
  efa <- psych::fa(attitude, nfactors = 3)
  params <- parameters::model_parameters(efa)
  expect_identical(c(nrow(params), ncol(params)), c(7L, 6L))

  model1 <- efa_to_cfa(efa)
  model2 <- efa_to_cfa(efa, threshold = 0.3)

  expect_identical(nchar(model1), 109L)

  m1 <- suppressWarnings(lavaan::cfa(model1, data = attitude))
  params <- parameters::model_parameters(m1)
  expect_identical(c(nrow(params), ncol(params)), c(10L, 10L))

  expect_message(parameters::model_parameters(m1, ci = c(0.8, 0.9)))

  params <- parameters::model_parameters(m1, standardize = TRUE, component = "all")
  expect_identical(c(nrow(params), ncol(params)), c(20L, 10L))

  x <- lavaan::anova(m1, lavaan::cfa(model2, data = attitude))
  params <- parameters::model_parameters(x)
  expect_identical(c(nrow(params), ncol(params)), c(2L, 6L))
})

test_that("FactoMineR", {
  skip_if_not_installed("FactoMineR")
  x <- suppressWarnings(model_parameters(FactoMineR::PCA(mtcars, ncp = 3), threshold = 0.2, sort = TRUE))
  expect_identical(c(ncol(x), nrow(x)), c(5L, 11L))

  x <- suppressWarnings(model_parameters(FactoMineR::FAMD(iris, ncp = 3), threshold = 0.2, sort = TRUE))
  expect_identical(c(ncol(x), nrow(x)), c(5L, 5L))
})

test_that("BayesFM", {
  skip_if_not_installed("BayesFM")
  set.seed(333)
  befa <- BayesFM::befa(mtcars, iter = 1000, verbose = FALSE)
  params <- suppressWarnings(parameters::model_parameters(befa, sort = TRUE))
  expect_identical(nrow(params), 11L)
})
