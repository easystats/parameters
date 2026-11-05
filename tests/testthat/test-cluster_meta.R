# Share of solutions that put each pair together, computed pair by pair
.meta_reference <- function(solutions) {
  n <- length(solutions[[1]])
  m <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      same <- vapply(
        solutions,
        function(s) {
          if (is.na(s[i]) || is.na(s[j]) || s[i] == "0" || s[j] == "0") {
            NA
          } else {
            s[i] == s[j]
          }
        },
        logical(1)
      )
      m[i, j] <- if (i == j) 1 else mean(same, na.rm = TRUE)
    }
  }
  m
}


test_that("cluster_meta, probability of being in the same cluster", {
  s1 <- c(1, 1, 1, 2, 2, 2)
  s2 <- c(1, 1, 2, 2, 3, 3)
  s3 <- c(1, 2, 1, 2, 1, 2)
  m <- cluster_meta(list(s1, s2, s3))

  expect_s3_class(m, "cluster_meta")
  expect_identical(dim(m), c(6L, 6L))
  expect_identical(dimnames(m), list(as.character(1:6), as.character(1:6)))
  expect_true(isSymmetric(unclass(m)))
  expect_identical(unname(diag(m)), rep(1, 6))

  # all three solutions count (the first one was ignored before)
  expect_equal(m["1", "2"], 2 / 3, tolerance = 1e-10) # together in s1, s2
  expect_equal(m["1", "3"], 2 / 3, tolerance = 1e-10) # together in s1, s3
  expect_equal(m["1", "4"], 0, tolerance = 1e-10) # never together
  expect_equal(m["5", "6"], 2 / 3, tolerance = 1e-10) # together in s1, s2
  expect_equal(m["4", "6"], 2 / 3, tolerance = 1e-10) # together in s1, s3
  expect_equal(
    unclass(m),
    .meta_reference(list(s1, s2, s3)),
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
})


test_that("cluster_meta, unassigned observations (NA or 0) are ignored", {
  s1 <- c(1, 1, 2, 2, 2)
  s2 <- c(0, 1, 1, 2, 2)
  s3 <- c("a", NA, "b", "b", "a")
  m <- cluster_meta(list(s1, s2, s3), rownames = letters[1:5])

  expect_identical(rownames(m), letters[1:5])
  expect_identical(colnames(m), letters[1:5])
  expect_equal(m["a", "b"], 1, tolerance = 1e-10) # only s1 assigns both
  expect_equal(m["c", "d"], 2 / 3, tolerance = 1e-10)
  expect_equal(m["a", "e"], 1 / 2, tolerance = 1e-10) # s1 apart, s3 together
  expect_equal(
    unclass(m),
    .meta_reference(list(s1, s2, s3)),
    tolerance = 1e-10,
    ignore_attr = TRUE
  )

  # a pair that no solution assigns is NaN
  m <- cluster_meta(list(c(1, 1, 2), c(NA, NA, 1)))
  expect_identical(m["1", "2"], 1)
  m <- cluster_meta(list(c(NA, 1, 2), c(1, NA, 1)))
  expect_true(is.nan(m["1", "2"]))
})


test_that("cluster_meta, works with cluster_analysis() and predict()", {
  data <- iris[c(1:10, 51:60, 101:110), 1:4]
  solutions <- withr::with_seed(
    1,
    list(
      cluster_analysis(data, n = 2, method = "kmeans"),
      cluster_analysis(data, n = 3, method = "kmeans"),
      cluster_analysis(data, n = 4, method = "kmeans")
    )
  )
  m <- cluster_meta(solutions)
  expect_identical(rownames(m), as.character(1:30))
  expect_equal(
    unclass(m),
    .meta_reference(lapply(solutions, predict)),
    tolerance = 1e-10,
    ignore_attr = TRUE
  )

  # predict() clusters the pairs that are often together
  groups <- predict(m, n = 2)
  expect_length(groups, 30)
  together <- outer(groups, groups, "==")
  expect_gt(mean(m[together]), mean(m[!together]))
})


test_that("cluster_meta, three clear groups are recovered by predict()", {
  s1 <- rep(1:3, each = 5)
  s2 <- rep(c(2, 3, 1), each = 5)
  s3 <- c(rep(1, 10), rep(2, 5))
  m <- cluster_meta(list(s1, s2, s3))
  expect_identical(as.vector(predict(m, n = 3)), s1)
  expect_identical(as.vector(predict(m, n = 3, method = "complete")), s1)
})


test_that("predict.cluster_meta, linkage method and tree", {
  # three solutions: the shares are 0, 1/3, 2/3 or 1, so many distances tie
  s1 <- c(1, 2, 1, 1, 2, 1, 1, 1)
  s2 <- c(2, 2, 3, 3, 1, 1, 1, 2)
  s3 <- c(2, 2, 2, 1, 1, 1, 1, 1)
  m <- cluster_meta(list(s1, s2, s3))
  d <- stats::as.dist(1 - unclass(m))

  # the default is average linkage
  out <- predict(m, n = 2)
  tree <- attributes(out)$tree
  expect_s3_class(tree, "hclust")
  expect_identical(tree$method, "average")
  expect_identical(tree$merge, stats::hclust(d, method = "average")$merge)
  expect_identical(
    as.vector(out),
    as.vector(stats::cutree(stats::hclust(d, method = "average"), k = 2))
  )
  expect_identical(names(out), as.character(1:8))

  # the method is passed to hclust()
  complete <- predict(m, n = 2, method = "complete")
  expect_identical(attributes(complete)$tree$method, "complete")
  expect_identical(
    as.vector(complete),
    as.vector(stats::cutree(stats::hclust(d, method = "complete"), k = 2))
  )

  # with ties, average and complete linkage give different partitions here
  expect_identical(as.vector(out), c(1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L))
  expect_identical(as.vector(complete), c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L))

  expect_error(predict(m), "number of clusters")
})
