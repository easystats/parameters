.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
  if (requiet("testthat") &&
    requiet("parameters") &&
    requiet("lme4") &&
    requiet("insight")) {
    data(mtcars)
    test_that("p_value", {
      # h-tests
      model <- insight::download_model("htest_1")
      expect_equal(p_value(model), 0.04136799, tolerance = 0.01)

      model <- insight::download_model("htest_2")
      expect_equal(p_value(model), 0.1518983, tolerance = 0.01)

      model <- insight::download_model("htest_3")
      expect_equal(p_value(model), 0.182921, tolerance = 0.01)

      model <- insight::download_model("htest_4")
      expect_equal(p_value(model), 0, tolerance = 0.01)

      model <- insight::download_model("htest_5")
      expect_equal(p_value(model), 0, tolerance = 0.01)

      model <- insight::download_model("htest_6")
      expect_equal(p_value(model), 0, tolerance = 0.01)

      model <- insight::download_model("htest_7")
      expect_equal(p_value(model), 0, tolerance = 0.01)

      model <- insight::download_model("htest_8")
      expect_equal(p_value(model), 0, tolerance = 0.01)

      # ANOVAs
      model <- insight::download_model("aov_1")
      expect_equal(p_value(model)$p, 0, tolerance = 0.01)

      model <- insight::download_model("anova_1")
      expect_equal(p_value(model)$p, 0, tolerance = 0.01)

      model <- insight::download_model("aovlist_1")
      expect_equal(p_value(model)$p, 0, tolerance = 0.01)

      model <- insight::download_model("aov_2")
      expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

      model <- insight::download_model("anova_2")
      expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

      model <- insight::download_model("aovlist_2")
      expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

      model <- insight::download_model("aov_3")
      expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

      model <- insight::download_model("anova_3")
      expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

      model <- insight::download_model("aovlist_3")
      expect_equal(p_value(model)$p[1], 0, tolerance = 0.01)

      model <- insight::download_model("anova_4")
      expect_equal(p_value(model)$p[2], 0, tolerance = 0.01)

      # ANOVA lmer
      model <- insight::download_model("anova_lmerMod_0")
      expect_equal(p_value(model), NA)

      model <- insight::download_model("anova_lmerMod_1")
      expect_equal(p_value(model), NA)

      model <- insight::download_model("anova_lmerMod_2")
      expect_equal(p_value(model), NA)

      model <- insight::download_model("anova_lmerMod_3")
      expect_equal(p_value(model), NA)

      model <- insight::download_model("anova_lmerMod_4")
      expect_equal(p_value(model), NA)

      model <- insight::download_model("anova_lmerMod_5")
      expect_equal(p_value(model), NA)

      model <- insight::download_model("anova_lmerMod_6")
      expect_equal(p_value(model)$p[2], 0, tolerance = 0.01)



      # Mixed models
      model <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
      expect_equal(p_value(model)$p[1], 0.206219, tolerance = 0.01)
      expect_equal(p_value(model, method = "normal")$p[1], 0.1956467, tolerance = 0.01)
      expect_equal(p_value(model, method = "kr")$p[1], 0.319398, tolerance = 0.01)

      model <- insight::download_model("merMod_1")
      expect_equal(p_value(model)$p[1], 0.06578, tolerance = 0.01)

      model <- insight::download_model("merMod_2")
      expect_equal(p_value(model)$p[1], 0.29912, tolerance = 0.01)
    })
  }
}
