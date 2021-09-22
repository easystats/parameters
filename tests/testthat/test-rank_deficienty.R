if (requiet("testthat") && requiet("parameters")) {
  set.seed(123)
  data(mtcars)
  model <-
    stats::lm(
      formula = wt ~ am * cyl * vs,
      data = mtcars
    )

  test_that("model_parameters-rank_deficiency", {
    expect_warning(model_parameters(model))
    params <- suppressWarnings(model_parameters(model))
    expect_equal(params$Parameter, c("(Intercept)", "am", "cyl", "vs", "am:cyl", "am:vs"), tolerance = 1e-3)
    expect_equal(params$Coefficient, c(2.28908, -1.37908, 0.22688, -0.26158, 0.08062, 0.14987), tolerance = 1e-3)
  })
}
