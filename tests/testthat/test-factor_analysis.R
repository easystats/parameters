test_that("factor_analysis", {
  skip_on_cran()
  skip_if_not_installed("GPArotation")
  skip_if_not_installed("psych")
  skip_if_not_installed("discovr")
  skip_if_not_installed("knitr")

  set.seed(333)

  raq_items <- as.data.frame(discovr::raq)
  raq_items$id <- NULL

  out <- factor_analysis(
    raq_items,
    n = 4,
    scores = "tenBerge",
    cor = "poly",
    rotation = "oblimin",
    threshold = 0.4,
    standardize = FALSE
  )
  raq_fa <- psych::fa(r = raq_items, nfactors = 4, scores = "tenBerge", cor = "poly")

  expect_equal(
    out$MR1,
    raq_fa$loadings[, "MR1"],
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  s <- summary(out)
  expect_equal(
    as.matrix(as.data.frame(s)[2, -1]),
    raq_fa$Vaccounted[2, ],
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  # include factor correlations
  out <- factor_analysis(
    mtcars[, 1:7],
    n = 2,
    rotation = "oblimin",
    threshold = "max",
    standardize = TRUE,
    sort = TRUE
  )
  expect_snapshot(print(summary(out)))
  expect_snapshot(print_md(summary(out)))

  # check factor scores
  fc <- factor_scores(out)
  expect_identical(dim(fc), c(32L, 2L))

  # works with correlation matrix
  skip_if_not_installed("correlation")
  skip_on_cran() # takes too long on CRAN

  raq_poly <- correlation::correlation(raq_items, method = "polychoric")
  raq_poly_mtx <- as.matrix(raq_poly) # store correlation matrix

  # needs n_obs
  expect_error(
    factor_analysis(raq_poly_mtx, n = 4),
    regex = "You provided a square matrix"
  )

  out1 <- factor_analysis(raq_poly_mtx, n = 4, n_obs = 2571)
  expect_identical(dim(out1), c(23L, 7L))
  expect_named(
    out1,
    c("Variable", "MR1", "MR2", "MR4", "MR3", "Complexity", "Uniqueness")
  )

  out2 <- factor_analysis(as.matrix(raq_items), n = 4)
  expect_identical(dim(out2), c(23L, 7L))
  expect_named(
    out2,
    c("Variable", "MR1", "MR2", "MR4", "MR3", "Complexity", "Uniqueness")
  )

  # roughly equal results
  expect_equal(out1$MR1, out2$MR1, tolerance = 1e-1)

  # text matrix n_obs
  williams <- as.data.frame(discovr::williams)
  williams$ID <- NULL
  n <- 28
  r <- correlation::correlation(williams[1:n])

  # create r-matrix
  r_mat <- matrix(0, nrow = n, ncol = n)
  diag(r_mat) <- 1
  r_mat[lower.tri(r_mat)] <- r$r
  r_mat[upper.tri(r_mat)] <- r$r

  # create n-matrix
  n_mat <- matrix(0, nrow = n, ncol = n)
  diag(n_mat) <- 1
  n_mat[lower.tri(n_mat)] <- r$n_Obs
  n_mat[upper.tri(n_mat)] <- r$n_Obs

  out <- suppressWarnings(factor_analysis(r_mat, n_obs = n_mat, n = 2))
  expect_identical(dim(out), c(28L, 5L))
  expect_named(
    out,
    c("Variable", "MR1", "MR2", "Complexity", "Uniqueness")
  )

  n_mat <- matrix(0, nrow = n - 2, ncol = n - 2)
  diag(n_mat) <- 1
  n_mat[lower.tri(n_mat)] <- r$n_Obs[1:325]
  n_mat[upper.tri(n_mat)] <- r$n_Obs[1:325]

  # matrix dimensions do not match
  expect_error(
    suppressWarnings(factor_analysis(r_mat, n_obs = n_mat, n = 2)),
    regex = "The provided"
  )
})


test_that("omega", {
  skip_on_cran()
  skip_if_not_installed("GPArotation")
  skip_if_not_installed("psych")

  model <- psych::omega(mtcars, nfactors = 3, plot = FALSE)
  out <- model_parameters(model)
  expect_snapshot(print(out))
  expect_snapshot(print_md(out))

  expect_snapshot(print(summary(out)))
  expect_snapshot(print_md(summary(out)))
})
