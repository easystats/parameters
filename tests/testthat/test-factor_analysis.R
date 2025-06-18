test_that("n_factors, default", {
  skip_on_cran()
  skip_if_not_installed("GPArotation")
  skip_if_not_installed("psych")
  skip_if_not_installed("discovr")

  set.seed(333)

  raq_items <- as.data.frame(discovr::raq)
  raq_items$id <- NULL

  out <- parameters::factor_analysis(
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
})
