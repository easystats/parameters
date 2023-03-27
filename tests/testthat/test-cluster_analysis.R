test_that("cluster_analysis, predict, matrix", {
  data(iris)
  iris.dat <- iris[c(1:15, 51:65, 101:115), -5]

  set.seed(123)
  iris.dat.km <- cluster_analysis(iris.dat, n = 4, method = "kmeans")
  x1 <- predict(iris.dat.km)

  set.seed(123)
  iris.mat <- as.matrix(iris.dat)
  iris.mat.km <- cluster_analysis(iris.mat, n = 4, method = "kmeans")
  x2 <- predict(iris.mat.km)

  expect_identical(x1, x2)
})

test_that("cluster_analysis, works with include_factors, #847", {
  d <- iris[3:5]
  rz_kmeans <- cluster_analysis(d, n = 3, method = "kmeans", include_factors = TRUE)
  expect_identical(rz_kmeans$Cluster, c("1", "2", "3"))
})
