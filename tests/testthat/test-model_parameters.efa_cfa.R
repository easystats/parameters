linux <- tryCatch({
  si <- Sys.info()
  if (!is.null(si["sysname"])) {
    si["sysname"] == "Linux" || grepl("^linux", R.version$os)
  } else {
    FALSE
  }
})


if (require("testthat") &&
  require("parameters") &&
  require("psych") &&
  suppressPackageStartupMessages(require("lavaan", quietly = TRUE)) &&
  suppressPackageStartupMessages(require("BayesFM", quietly = TRUE)) &&
  require("FactoMineR")) {
  test_that("principal_components", {
    set.seed(333)

    x <- principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
    expect_equal(c(ncol(x), nrow(x)), c(8, 7))
    x <- principal_components(mtcars[, 1:7], n = 2, rotation = "oblimin", threshold = "max", sort = TRUE)
    expect_equal(c(ncol(x), nrow(x)), c(6, 7))

    pca <- principal_components(mtcars[, 1:5], n = 2)
    expect_equal(c(ncol(pca), nrow(pca)), c(4, 5))
    x <- summary(pca)
    expect_equal(c(ncol(x), nrow(x)), c(3, 4))
    x <- model_parameters(pca)
    expect_equal(c(ncol(x), nrow(x)), c(5, 2))
    x <- predict(pca)
    expect_equal(c(ncol(x), nrow(x)), c(2, 32))
  })



  test_that("efa-cfa", {
    efa <- psych::fa(attitude, nfactors = 3)
    params <- parameters::model_parameters(efa)
    expect_equal(c(nrow(params), ncol(params)), c(7, 6))

    model1 <- efa_to_cfa(efa)
    model2 <- efa_to_cfa(efa, threshold = 0.3)

    expect_equal(nchar(model1), 109)

    m1 <- suppressWarnings(lavaan::cfa(model1, data = attitude))
    params <- parameters::model_parameters(m1)
    expect_equal(c(nrow(params), ncol(params)), c(10, 10))

    expect_warning(parameters::model_parameters(m1, ci = c(0.8, 0.9)))

    params <- parameters::model_parameters(m1, standardize = TRUE, component = "all")
    expect_equal(c(nrow(params), ncol(params)), c(20, 10))

    x <- lavaan::anova(m1, lavaan::cfa(model2, data = attitude))
    params <- parameters::model_parameters(x)
    expect_equal(c(nrow(params), ncol(params)), c(2, 6))
  })


  # if (!linux) {
  #   test_that("FactoMineR", {
  #     x <- suppressWarnings(model_parameters(FactoMineR::PCA(mtcars, ncp = 3), threshold = 0.2, sort = TRUE))
  #     expect_equal(c(ncol(x), nrow(x)), c(5, 11))
  #
  #     x <- suppressWarnings(model_parameters(FactoMineR::FAMD(iris, ncp = 3), threshold = 0.2, sort = TRUE))
  #     expect_equal(c(ncol(x), nrow(x)), c(5, 5))
  #   })
  # }


  set.seed(333)
  befa <- BayesFM::befa(mtcars, iter = 1000, verbose = FALSE)
  params <- suppressWarnings(parameters::model_parameters(befa, sort = TRUE))

  test_that("BayesFM", {
    expect_equal(nrow(params), 11)
  })
}
