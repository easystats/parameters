.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("testthat") && require("parameters")) {
  test_that("emmeans | lm", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("boot")

    model <- lm(mpg ~ log(wt) + factor(cyl), data = mtcars)

    set.seed(7)
    b <- bootstrap_model(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~ cyl))$emmean,
                 summary(emmeans::emmeans(model, ~ cyl))$emmean,
                 tolerance = 0.1)

    set.seed(7)
    b <- bootstrap_parameters(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~ cyl))$emmean,
                 summary(emmeans::emmeans(model, ~ cyl))$emmean,
                 tolerance = 0.1)

    mp <- model_parameters(emmeans::emmeans(b, consec ~ cyl))
    expect_true("p" %in% colnames(mp))
    expect_true("SE" %in% colnames(mp))
    expect_equal(nrow(mp), 5)
  })


  test_that("emmeans | lmer", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("boot")
    skip_if_not_installed("lme4")

    model <- lme4::lmer(mpg ~ log(wt) + factor(cyl) + (1|gear), data = mtcars)

    set.seed(7)
    b <- bootstrap_model(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~ cyl))$emmean,
                 summary(emmeans::emmeans(model, ~ cyl))$emmean,
                 tolerance = 0.1)

    set.seed(7)
    b <- bootstrap_parameters(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~ cyl))$emmean,
                 summary(emmeans::emmeans(model, ~ cyl))$emmean,
                 tolerance = 0.1)

    mp <- model_parameters(emmeans::emmeans(b, consec ~ cyl))
    expect_true("p" %in% colnames(mp))
    expect_true("SE" %in% colnames(mp))
    expect_equal(nrow(mp), 5)
  })
}
