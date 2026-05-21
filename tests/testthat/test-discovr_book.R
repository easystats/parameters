skip_on_os("mac")
skip_on_cran()
skip_if_not_installed("rstanarm")
skip_if_not_installed("discovr")

test_that("mp, test bf and pd, chapter 8.14", {
  set.seed(123)
  m <- stan_glm(sales ~ adverts + airplay + image, data = discovr::album_sales)
  out <- suppressWarnings(model_parameters(m, test = c("pd", "bf"), null = 0))

  expect_identical(
    capture.output(out),
    c(
      "Parameter   | Median |          95% CI |     pd |       BF |  Rhat | ESS (tail) |                     Prior",
      "-----------------------------------------------------------------------------------------------------------",
      "(Intercept) | -26.70 | [-59.82,  5.46] | 94.53% |    0.051 | 1.000 |       3252 | Normal (193.20 +- 201.75)",
      "adverts     |   0.08 | [  0.07,  0.10] |   100% | 4.65e+10 | 1.000 |       3038 |     Normal (0.00 +- 0.42)",
      "airplay     |   3.36 | [  2.83,  3.92] |   100% | 4.85e+11 | 1.000 |       2980 |    Normal (0.00 +- 16.44)",
      "image       |  11.11 | [  6.48, 15.82] |   100% |    88.36 | 1.000 |       3309 |   Normal (0.00 +- 144.59)"
    )
  )
})
