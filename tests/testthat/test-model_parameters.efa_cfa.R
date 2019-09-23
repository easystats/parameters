context("efa-cfa")


test_that("principal_components", {
  set.seed(333)

  x <- principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
  testthat::expect_equal(c(ncol(x), nrow(x)), c(8, 7))
  x <- principal_components(mtcars[, 1:7], n = 2, rotation = "oblimin", threshold = "max", sort = TRUE)
  testthat::expect_equal(c(ncol(x), nrow(x)), c(5, 7))

  pca <- principal_components(mtcars[, 1:5], n = 2)
  testthat::expect_equal(c(ncol(pca), nrow(pca)), c(4, 5))
  x <- summary(pca)
  testthat::expect_equal(c(ncol(x), nrow(x)), c(3, 4))
  x <- model_parameters(pca)
  testthat::expect_equal(c(ncol(x), nrow(x)), c(5, 2))
  x <- predict(pca)
  testthat::expect_equal(c(ncol(x), nrow(x)), c(2, 32))
})



test_that("efa-cfa", {
  library(psych)
  library(lavaan)

  efa <- psych::fa(attitude, nfactors = 3)
  params <- parameters::model_parameters(efa)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(7, 6))

  model1 <- efa_to_cfa(efa)
  model2 <- efa_to_cfa(efa, threshold = 0.3)

  testthat::expect_equal(nchar(model1), 109)

  m1 <- lavaan::cfa(model1, data = attitude)
  params <- parameters::model_parameters(m1)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(10, 9))

  testthat::expect_warning(parameters::model_parameters(m1, ci = c(0.8, 0.9)))

  params <- parameters::model_parameters(m1, standardize = TRUE, type = "all")
  testthat::expect_equal(c(nrow(params), ncol(params)), c(20, 9))

  x <- lavaan::anova(m1, lavaan::cfa(model2, data = attitude))
  params <- parameters::model_parameters(x)
  testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 6))
})



test_that("FactoMineR", {
  library(FactoMineR)

  x <- model_parameters(FactoMineR::PCA(mtcars, ncp = 3), threshold = 0.2, sort = TRUE)
  testthat::expect_equal(c(ncol(x), nrow(x)), c(4, 11))

  x <- model_parameters(FactoMineR::FAMD(iris, ncp = 3), threshold = 0.2, sort = TRUE)
  testthat::expect_equal(c(ncol(x), nrow(x)), c(4, 5))
})



test_that("BayesFM", {
  set.seed(333)
  library(BayesFM)

  befa <- BayesFM::befa(mtcars, iter = 1000)
  params <- parameters::model_parameters(befa, sort = TRUE)
  testthat::expect_equal(nrow(params), 11)
})
