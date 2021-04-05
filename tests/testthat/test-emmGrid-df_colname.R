.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
    require("testthat") &&
    require("parameters") &&
    require("emmeans") &&
    require("lme4")) {

  data(sleep)
  data(fiber)

  m <- lm(strength ~ diameter + machine, data = fiber)
  emm <- emmeans(m, "machine")
  es1 <- eff_size(emm, sigma = sigma(m), edf = df.residual(m))

  sleep$group <- as.factor(sleep$group)
  m2 <- lme4::lmer(extra ~ group + (1 | ID), sleep)
  emm2 <- emmeans(m2, ~group, df = NA)
  es2 <- eff_size(emm2, sigma = sigma(m2), edf = df.residual(m2))

  test_that("df", {
    expect_equal(colnames(model_parameters(es1)),
                 c("contrast", "Coefficient", "SE", "CI", "CI_low", "CI_high",
                   "t", "df_error", "p"))

    expect_equal(colnames(model_parameters(es2)),
                 c("contrast", "Coefficient", "SE", "CI", "CI_low", "CI_high",
                   "z", "df_error", "p"))
  })
}