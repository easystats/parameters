.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && requiet("posterior")) {
  model <- insight::download_model("brms_1")

  test_that("mp-posterior-draws", {
    x <- posterior::as_draws(model)
    mp <- model_parameters(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  })

  test_that("mp-posterior-draws_list", {
    x <- posterior::as_draws_list(model)
    mp <- model_parameters(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  })

  test_that("mp-posterior-draws_df", {
    x <- posterior::as_draws_df(model)
    mp <- model_parameters(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  })

  test_that("mp-posterior-draws_matrix", {
    x <- posterior::as_draws_matrix(model)
    mp <- model_parameters(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  })

  test_that("mp-posterior-draws_array", {
    x <- posterior::as_draws_array(model)
    mp <- model_parameters(x)
    expect_equal(mp$Median, c(39.68234, -3.19505, -1.4936, 2.62881, -79.73344), tolerance = 1e-2, ignore_attr = TRUE)
    expect_equal(mp$Parameter, c("b_Intercept", "b_wt", "b_cyl", "sigma", "lp__"))
  })
}
