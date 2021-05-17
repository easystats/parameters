.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("testthat") && require("parameters")) {
  test_that("emmeans | lm", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("boot")

    model <- lm(mpg ~ log(wt) + factor(cyl), data = mtcars)

    set.seed(7)
    b <- bootstrap_model(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~cyl))$emmean,
      summary(emmeans::emmeans(model, ~cyl))$emmean,
      tolerance = 0.1
    )

    set.seed(7)
    b <- bootstrap_parameters(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~cyl))$emmean,
      summary(emmeans::emmeans(model, ~cyl))$emmean,
      tolerance = 0.1
    )

    mp <- model_parameters(emmeans::emmeans(b, consec ~ cyl), verbose = FALSE)
    expect_equal(colnames(mp),
                 c("Parameter", "Mean", "SD", "CI", "CI_low", "CI_high", "pd",
                   "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Component"))
    expect_equal(nrow(mp), 5)
  })


  test_that("emmeans | lmer", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("boot")
    skip_if_not_installed("lme4")

    model <- lme4::lmer(mpg ~ log(wt) + factor(cyl) + (1 | gear), data = mtcars)

    set.seed(7)
    b <- bootstrap_model(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~cyl))$emmean,
      summary(emmeans::emmeans(model, ~cyl))$emmean,
      tolerance = 0.1
    )

    set.seed(7)
    b <- bootstrap_parameters(model, iterations = 1000)
    expect_equal(summary(emmeans::emmeans(b, ~cyl))$emmean,
      summary(emmeans::emmeans(model, ~cyl))$emmean,
      tolerance = 0.1
    )

    mp <- suppressWarnings(model_parameters(emmeans::emmeans(b, consec ~ cyl)))
    expect_equal(colnames(mp),
                 c("Parameter", "Mean", "SD", "CI", "CI_low", "CI_high", "pd",
                   "ROPE_CI", "ROPE_low", "ROPE_high", "ROPE_Percentage", "Component"))
    expect_equal(nrow(mp), 5)
  })
}
