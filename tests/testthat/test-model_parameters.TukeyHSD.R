x1 <- TukeyHSD(aov(breaks ~ wool + tension, data = warpbreaks), "tension", ordered = TRUE)
x2 <- TukeyHSD(aov(breaks ~ wool * tension, data = warpbreaks), conf.level = 0.9)

test_that("model_parameters.TukeyHSD", {
  for (x in list(x1, x2)) {
    params <- model_parameters(x)
    expect_s3_class(params, "data.frame")
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

  params <- model_parameters(x1)
  expect_identical(nrow(params), 3L)
  expect_identical(params$Contrast, c("M-H", "L-H", "L-M"))
  expect_equal(params$Coefficient[1], 4.72, tolerance = 0.01)
  expect_equal(params$CI_low[1], -4.63, tolerance = 0.01)
  expect_equal(params$CI_high[1], 14.07, tolerance = 0.01)
  expect_equal(params$p[1], 0.447, tolerance = 0.01)

  params <- model_parameters(x2)
  expect_identical(nrow(params), 19L)
  expect_identical(unique(params$Term), c("wool", "tension", "wool:tension"))
  expect_equal(params$CI, rep(0.9, 19))
})

test_that("model_parameters.TukeyHSD, p_adjust, summary and plot", {
  params <- model_parameters(x1, p_adjust = "holm")
  expect_equal(params$p, unname(x1$tension[, "p adj"]))
  expect_identical(attr(params, "p_adjust"), "tukey")

  out <- capture.output(summary(model_parameters(x1)))
  expect_true(any(grepl("Contrast", out, fixed = TRUE)))
  expect_true(any(grepl("M-H", out, fixed = TRUE)))

  skip_if_not_installed("see")
  expect_s3_class(plot(model_parameters(x1)), "ggplot")
})

test_that("print model_parameters.TukeyHSD", {
  for (x in list(x1, x2)) {
    out <- capture.output(print(model_parameters(x)))
    expect_false(any(grepl("# Fixed Effects", out, fixed = TRUE)))
    out <- out[nzchar(trimws(out))]
    expect_identical(out[length(out)], "p-value adjustment method: Tukey")
  }
  expect_true(any(grepl(
    "95% CI",
    capture.output(print(model_parameters(x1))),
    fixed = TRUE
  )))
  out2 <- capture.output(print(model_parameters(x2)))
  expect_true(any(grepl("90% CI", out2, fixed = TRUE)))
  expect_false(any(grepl("95% CI", out2, fixed = TRUE)))

  out <- capture.output(print(model_parameters(x1, verbose = FALSE)))
  expect_false(any(grepl("p-value adjustment method", out, fixed = TRUE)))
  expect_true(any(grepl("M-H", out, fixed = TRUE)))
})

test_that("print model_parameters.TukeyHSD, snapshots", {
  expect_snapshot(print(model_parameters(x1)))
  expect_snapshot(print(model_parameters(x2)))
})
