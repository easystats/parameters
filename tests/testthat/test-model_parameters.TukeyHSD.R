x1 <- TukeyHSD(aov(breaks ~ wool + tension, data = warpbreaks), "tension", ordered = TRUE)
x2 <- TukeyHSD(aov(breaks ~ wool * tension, data = warpbreaks), conf.level = 0.9)

test_that("model_parameters.TukeyHSD", {
  for (x in list(x1, x2)) {
    params <- model_parameters(x)
    expect_named(
      params,
      c("Term", "Contrast", "Coefficient", "CI", "CI_low", "CI_high", "p")
    )

    terms <- rep(names(x), vapply(x, nrow, integer(1)))
    mats <- do.call(rbind, unclass(x))
    expect_identical(params$Term, terms)
    expect_identical(params$Contrast, rownames(mats))
    expect_equal(params$Coefficient, unname(mats[, "diff"]))
    expect_equal(params$CI_low, unname(mats[, "lwr"]))
    expect_equal(params$CI_high, unname(mats[, "upr"]))
    expect_equal(params$p, unname(mats[, "p adj"]))
    expect_equal(params$CI, rep(attr(x, "conf.level"), nrow(mats)))
  }
})

test_that("model_parameters.TukeyHSD, p_adjust", {
  params <- model_parameters(x1, p_adjust = "holm")
  expect_equal(params$p, unname(x1$tension[, "p adj"]))
  expect_identical(attr(params, "p_adjust"), "tukey")
})

test_that("print model_parameters.TukeyHSD", {
  out <- capture.output(print(model_parameters(x1, verbose = FALSE)))
  expect_false(any(grepl("p-value adjustment method", out, fixed = TRUE)))
})

test_that("print model_parameters.TukeyHSD, snapshots", {
  expect_snapshot(print(model_parameters(x1)))
  expect_snapshot(print(model_parameters(x2)))
})
