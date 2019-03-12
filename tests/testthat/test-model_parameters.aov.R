context("model_parameters.aov")

test_that("model_parameters.aov", {
  model <- circus::download_model("aov_1")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- circus::download_model("aov_2")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- circus::download_model("aov_3")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)
})


context("model_parameters.anova")

test_that("model_parameters.anova", {
  model <- circus::download_model("anova_1")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- circus::download_model("anova_2")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- circus::download_model("anova_3")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- circus::download_model("anova_4")
  testthat::expect_equal(sum(model_parameters(model)$DoF, na.rm = TRUE), 2)

  model <- circus::download_model("anova_lmerMod_0")
  testthat::expect_equal(nrow(model_parameters(model)), 0)

  model <- circus::download_model("anova_lmerMod_1")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 1)

  model <- circus::download_model("anova_lmerMod_2")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 2)

  model <- circus::download_model("anova_lmerMod_3")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 3)

  model <- circus::download_model("anova_lmerMod_4")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 2)

  model <- circus::download_model("anova_lmerMod_5")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 1)

  model <- circus::download_model("anova_lmerMod_6")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 12)
})




context("model_parameters.anova")

test_that("model_parameters.anova", {
  model <- circus::download_model("aovlist_1")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 31)

  model <- circus::download_model("aovlist_2")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)

  model <- circus::download_model("aovlist_3")
  testthat::expect_equal(sum(model_parameters(model)$DoF), 149)
})
