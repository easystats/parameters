skip_on_cran()

  skip_if_not_installed("emmeans")
  skip_if_not_installed("lme4")
  data(sleep, package = "lme4")
  data(fiber, package = "emmeans")

  m <- lm(strength ~ diameter + machine, data = fiber)
  emm <- emmeans::emmeans(m, "machine")
  es1 <- emmeans::eff_size(emm, sigma = sigma(m), edf = df.residual(m))

  sleep$group <- as.factor(sleep$group)
  m2 <- lme4::lmer(extra ~ group + (1 | ID), sleep)
  emm2 <- emmeans::emmeans(m2, ~group, df = NA)
  es2 <- emmeans::eff_size(emm2, sigma = sigma(m2), edf = df.residual(m2))

  test_that("df", {
    expect_equal(
      colnames(model_parameters(es1)),
      c(
        "contrast", "Coefficient", "SE", "CI", "CI_low", "CI_high",
        "t", "df_error", "p"
      )
    )

    expect_equal(
      colnames(model_parameters(es2)),
      c(
        "contrast", "Coefficient", "SE", "CI", "CI_low", "CI_high",
        "z", "df_error", "p"
      )
    )
  })

  test_that("print model_parameters", {
    mp <- model_parameters(emm)
    expect_snapshot(mp)

    mp <- model_parameters(es1)
    expect_snapshot(mp)
  })

