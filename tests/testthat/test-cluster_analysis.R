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

test_that("cluster_analysis, hkmeans", {
  x <- datawizard::standardize(iris[1:4])
  x[] <- lapply(x, as.numeric)
  rez <- cluster_analysis(iris[1:4], n = 3, method = "hkmeans")
  model <- attributes(rez)$model
  expect_s3_class(model, "hkmeans")

  # k-means started from the centers of the hierarchical clusters
  groups <- stats::cutree(stats::hclust(stats::dist(x), method = "complete"), k = 3)
  centers <- stats::aggregate(x, list(groups), mean)[, -1]
  km <- stats::kmeans(x, centers = centers, iter.max = 100)
  expect_identical(unname(model$cluster), unname(km$cluster))
  expect_identical(predict(rez), unname(km$cluster))
})

test_that("cluster_analysis, hkmeans keeps the kmeans class and uses hc.metric", {
  x <- datawizard::standardize(iris[1:4])
  x[] <- lapply(x, as.numeric)

  rez <- cluster_analysis(iris[1:4], n = 3, method = "hkmeans")
  model <- attributes(rez)$model
  expect_s3_class(model, c("hkmeans", "kmeans"), exact = TRUE)
  # predict() on new data works (it uses the kmeans method)
  expect_identical(predict(model, newdata = x), unname(model$cluster))

  # the distance of the hierarchical clustering can be set with `hc.metric`
  rez <- cluster_analysis(iris[1:4], n = 3, method = "hkmeans", hc.metric = "manhattan")
  hc <- stats::hclust(stats::dist(x, method = "manhattan"), method = "complete")
  expect_identical(attributes(rez)$model$hclust$merge, hc$merge)
  groups <- stats::cutree(hc, k = 3)
  centers <- stats::aggregate(x, list(groups), mean)[, -1]
  km <- stats::kmeans(x, centers = centers, iter.max = 100)
  expect_identical(predict(rez), unname(km$cluster))
})
