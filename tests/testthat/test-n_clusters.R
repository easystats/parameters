.four_blobs <- function() {
  # Four separated groups. With this seed, the best mclust solutions are
  # EII,4 (best), EII,1 and VII,1.
  withr::with_seed(29, {
    data.frame(
      a = c(rnorm(40, 0), rnorm(40, 3), rnorm(40, 0), rnorm(40, 3)),
      b = c(rnorm(40, 0), rnorm(40, 0), rnorm(40, 3), rnorm(40, 3))
    )
  })
}


test_that("n_clusters, mclust gives one vote from the best BIC solution", {
  skip_on_cran()
  skip_if_not_installed("mclust")
  x <- .four_blobs()

  out <- n_clusters(x, package = "mclust", n_max = 6)

  # Previously, the top 3 solutions were each a vote, and the duplicate
  # removal kept the smallest k of a model (here EII = 1) instead of the best.
  expect_identical(nrow(out), 1L)
  expect_identical(out$Method, "Mixture (EII)")
  expect_identical(out$n_Clusters, 4)
  expect_identical(attributes(out)$n, 4)

  # The best solution is the one that mclust reports as best
  xs <- as.data.frame(scale(x))
  mclustBIC <- mclust::mclustBIC
  best <- names(summary(mclust::mclustBIC(xs, G = 1:6, verbose = FALSE)))[1]
  expect_identical(paste0("EII,", out$n_Clusters), best)

  # The runner-ups are kept as an attribute
  bic <- attributes(out)$mclust_BIC
  expect_s3_class(bic, "data.frame")
  expect_named(bic, c("Model", "n_Clusters", "BIC", "BIC_diff"))
  expect_identical(nrow(bic), 3L)
  expect_identical(bic$Model[1], "EII")
  expect_identical(bic$n_Clusters[1], 4)
  expect_identical(bic$BIC_diff[1], 0)
  expect_true(all(bic$BIC_diff[-1] <= 0))
})


test_that("n_clusters, easystats methods give one vote each", {
  skip_on_cran()
  skip_if_not_installed("cluster")
  skip_if_not_installed("cluster")
  x <- .four_blobs()

  out <- withr::with_seed(1, n_clusters(x, package = "easystats", n_max = 6))

  expect_setequal(
    out$Method,
    c("Elbow", "Silhouette", "Gap_Maechler2012", "Gap_Dudoit2002")
  )
  expect_type(out$n_Clusters, "double")
  expect_true(all(out$n_Clusters %in% 1:6))
  # Both gap rules are applied to the same gap curve (computed once)
  gap <- out[startsWith(out$Method, "Gap"), ]
  expect_identical(gap$Duration[1], gap$Duration[2])
})


test_that("n_clusters, arguments for other methods do not drop NbClust votes", {
  skip_on_cran()
  skip_if_not_installed("NbClust")
  x <- .four_blobs()

  # `gap_method` is documented for n_clusters(). Passing it to NbClust() made
  # every NbClust call fail silently.
  out <- n_clusters(
    x,
    package = "NbClust",
    n_max = 6,
    indices = c("Ch", "DB"),
    gap_method = "globalSEmax"
  )
  expect_identical(sort(out$Method), c("Ch", "DB"))

  # Arguments that NbClust() accepts are still passed on
  out <- n_clusters(x, package = "NbClust", n_max = 6, indices = "Ch", min.nc = 3)
  expect_gte(out$n_Clusters, 3)
})


test_that("n_clusters, the silhouette and gap indices are counted once", {
  skip_on_cran()
  skip_if_not_installed("NbClust")
  skip_if_not_installed("cluster")
  x <- .four_blobs()

  out <- withr::with_seed(
    1,
    n_clusters(
      x,
      package = c("easystats", "NbClust"),
      n_max = 6,
      fast = FALSE,
      indices = c("Ch", "Silhouette", "gap")
    )
  )
  expect_identical(sum(out$Method == "Silhouette"), 1L)
  expect_identical(out$Package[out$Method == "Silhouette"], "easystats")
  expect_false("gap" %in% out$Method)
  expect_identical(sum(startsWith(out$Method, "Gap_")), 2L)
  expect_identical(out$Method[out$Package == "NbClust"], "Ch")

  # Without the easystats methods, the NbClust indices are kept
  out <- n_clusters(
    x,
    package = "NbClust",
    n_max = 6,
    fast = FALSE,
    indices = c("Silhouette", "gap")
  )
  expect_setequal(out$Method, c("Silhouette", "gap"))
  expect_identical(unique(out$Package), "NbClust")
})


test_that("n_clusters, NbClust does not reset the user's random seed", {
  skip_on_cran()
  skip_if_not_installed("NbClust")
  x <- .four_blobs()

  withr::local_seed(123)
  expected <- withr::with_preserve_seed(stats::runif(3))
  n_clusters(x, package = "NbClust", n_max = 6, indices = "Ch")
  # NbClust() calls set.seed(1); the next draws are the same as without it
  expect_identical(stats::runif(3), expected)
})


test_that("n_clusters, M3C does not reset the user's random seed", {
  skip_on_cran()
  skip_if_not_installed("M3C")
  x <- .four_blobs()

  withr::local_seed(123)
  expected <- withr::with_preserve_seed(stats::runif(3))
  n_clusters(x, package = "M3C", n_max = 4)
  # M3C() calls set.seed(); the next draws are the same as without it
  expect_identical(stats::runif(3), expected)
})


test_that(".restore_seed restores or removes the random seed", {
  withr::local_seed(1)
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  expected <- withr::with_preserve_seed(stats::runif(2))
  set.seed(99)
  .restore_seed(old_seed)
  expect_identical(stats::runif(2), expected)

  withr::with_preserve_seed({
    .restore_seed(NULL)
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })
})


test_that("n_clusters, `gap_method` does not break the easystats methods", {
  skip_on_cran()
  skip_if_not_installed("cluster")
  skip_if_not_installed("cluster")
  x <- .four_blobs()

  out <- withr::with_seed(
    1,
    n_clusters(x, package = "easystats", n_max = 6, gap_method = "globalSEmax")
  )
  expect_identical(nrow(out), 4L)
})


test_that("n_clusters_elbow returns a numeric number of clusters", {
  skip_on_cran()
  x <- .four_blobs()

  out <- withr::with_seed(1, n_clusters_elbow(x, n_max = 6))

  expect_type(out$n_Clusters, "double")
  expect_type(attributes(out)$n, "double")
  expect_identical(out$n_Clusters, as.numeric(1:6))

  # WSS is the within-cluster sum of squares of k-means
  xs <- as.matrix(scale(x))
  expect_equal(out$WSS[1], sum(scale(xs, scale = FALSE)^2), tolerance = 1e-10)
  km <- withr::with_seed(1, {
    stats::kmeans(xs, 1)
    stats::kmeans(xs, 2)
  })
  expect_equal(out$WSS[2], km$tot.withinss, tolerance = 1e-10)
})


test_that("n_clusters_silhouette and n_clusters_gap work with cluster::pam", {
  skip_on_cran()
  skip_if_not_installed("cluster")
  x <- .four_blobs()
  pam_k <- function(x, k) cluster::pam(x, k)

  out <- n_clusters_silhouette(x, n_max = 6, clustering_function = pam_k)
  expect_identical(out$Silhouette[1], 0)
  expect_true(all(out$Silhouette[-1] > 0 & out$Silhouette[-1] <= 1))
  expect_identical(attributes(out)$n, 4L)

  out <- withr::with_seed(1, n_clusters_gap(x, n_max = 6, clustering_function = pam_k))
  expect_named(out, c("n_Clusters", "Gap", "SE"))
  expect_equal(attributes(out)$ymin, out$Gap - out$SE, tolerance = 1e-10)
})
