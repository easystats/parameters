.runThisTest <- Sys.getenv("RunAllggeffectsTests") == "yes"


if (require("testthat") &&
    require("parameters") &&
    require("lme4") &&
    require("insight")) {
  data(mtcars)
  test_that("p_value", {
    # h-tests
    model <- insight::download_model("htest_1")
    testthat::expect_equal(p_value(model), 0.0413, tol = 0.01)

    model <- insight::download_model("htest_2")
    testthat::expect_equal(p_value(model), 0.151, tol = 0.01)

    model <- insight::download_model("htest_3")
    testthat::expect_equal(p_value(model), 0.183, tol = 0.01)

    model <- insight::download_model("htest_4")
    testthat::expect_equal(p_value(model), 0, tol = 0.01)

    model <- insight::download_model("htest_5")
    testthat::expect_equal(p_value(model), 0, tol = 0.01)

    model <- insight::download_model("htest_6")
    testthat::expect_equal(p_value(model), 0, tol = 0.01)

    model <- insight::download_model("htest_7")
    testthat::expect_equal(p_value(model), 0, tol = 0.01)

    model <- insight::download_model("htest_8")
    testthat::expect_equal(p_value(model), 0, tol = 0.01)

    # ANOVAs
    model <- insight::download_model("aov_1")
    testthat::expect_equal(p_value(model)$p, 0, tol = 0.01)

    model <- insight::download_model("anova_1")
    testthat::expect_equal(p_value(model)$p, 0, tol = 0.01)

    model <- insight::download_model("aovlist_1")
    testthat::expect_equal(p_value(model)$p, 0, tol = 0.01)

    model <- insight::download_model("aov_2")
    testthat::expect_equal(p_value(model)$p[1], 0, tol = 0.01)

    model <- insight::download_model("anova_2")
    testthat::expect_equal(p_value(model)$p[1], 0, tol = 0.01)

    model <- insight::download_model("aovlist_2")
    testthat::expect_equal(p_value(model)$p[1], 0.922, tol = 0.01)

    model <- insight::download_model("aov_3")
    testthat::expect_equal(p_value(model)$p[1], 0, tol = 0.01)

    model <- insight::download_model("anova_3")
    testthat::expect_equal(p_value(model)$p[1], 0, tol = 0.01)

    model <- insight::download_model("aovlist_3")
    testthat::expect_equal(p_value(model)$p[1], 0, tol = 0.01)

    model <- insight::download_model("anova_4")
    testthat::expect_equal(p_value(model)$p[2], 0, tol = 0.01)

    # ANOVA lmer
    model <- insight::download_model("anova_lmerMod_0")
    testthat::expect_equal(p_value(model), NA)

    model <- insight::download_model("anova_lmerMod_1")
    testthat::expect_equal(p_value(model), NA)

    model <- insight::download_model("anova_lmerMod_2")
    testthat::expect_equal(p_value(model), NA)

    model <- insight::download_model("anova_lmerMod_3")
    testthat::expect_equal(p_value(model), NA)

    model <- insight::download_model("anova_lmerMod_4")
    testthat::expect_equal(p_value(model), NA)

    model <- insight::download_model("anova_lmerMod_5")
    testthat::expect_equal(p_value(model), NA)

    model <- insight::download_model("anova_lmerMod_6")
    testthat::expect_equal(p_value(model)$p[2], 0, tol = 0.01)



    # Mixed models
    model <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
    testthat::expect_equal(p_value(model)$p[1], 0.195, tol = 0.01)
    testthat::expect_equal(p_value(model, method = "kr")$p[1], 0.227, tol = 0.01)

    model <- insight::download_model("merMod_1")
    testthat::expect_equal(p_value(model)$p[1], 0.065, tol = 0.01)

    model <- insight::download_model("merMod_2")
    testthat::expect_equal(p_value(model)$p[1], 0.299, tol = 0.01)
  })
}
