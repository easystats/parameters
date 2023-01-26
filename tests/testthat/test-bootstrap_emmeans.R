.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest) {
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
    expect_identical(
      colnames(mp),
      c("Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "Component")
    )
    expect_identical(nrow(mp), 5L)
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
    expect_identical(
      colnames(mp),
      c("Parameter", "Median", "CI", "CI_low", "CI_high", "pd", "Component")
    )
    expect_identical(nrow(mp), 5L)
  })


  if (requiet("emmeans") && requiet("boot") && requiet("lme4") && requiet("glmmTMB")) {
    test_that("emmeans | glmmTMB", {
      data(Salamanders)
      model <- glmmTMB(count ~ spp + mined + (1 | site), family = nbinom2, data = Salamanders)

      set.seed(7)
      b <- bootstrap_parameters(model, iterations = 10)
      out <- summary(emmeans(b, ~spp, type = "response"))

      expect_equal(
        out$response,
        c(0.654, 0.1515, 0.8856, 0.261, 0.9775, 1.2909, 0.9031),
        tolerance = 0.1
      )

      expect_identical(
        colnames(out),
        c("spp", "response", "lower.HPD", "upper.HPD")
      )
      expect_identical(nrow(out), 7L)
    })
  }
}
