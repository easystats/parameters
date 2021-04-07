.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
    require("testthat") &&
    require("parameters") &&
    require("emmeans") &&
    require("lme4") &&
    packageVersion("insight") > "0.13.2") {

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

  test_that("print model_parameters", {
    mp <- model_parameters(emm)
    out <- capture.output(print(mp))
    expect_equal(
      out,
      c("machine | Coefficient |   SE |         95% CI | t(11) |      p",
        "--------------------------------------------------------------",
        "A       |       40.38 | 0.72 | [38.79, 41.98] | 55.81 | < .001",
        "B       |       41.42 | 0.74 | [39.78, 43.06] | 55.64 | < .001",
        "C       |       38.80 | 0.79 | [37.06, 40.53] | 49.24 | < .001"
      ))

    mp <- model_parameters(es1)
    out <- capture.output(print(mp))
    expect_equal(
      out,
      c("contrast | Coefficient |   SE |        95% CI | t(11) |     p",
        "-------------------------------------------------------------",
        "A - B    |       -0.65 | 0.65 | [-2.08, 0.78] | -1.00 | 0.339",
        "A - C    |        0.99 | 0.73 | [-0.60, 2.59] |  1.37 | 0.198",
        "B - C    |        1.64 | 0.80 | [-0.12, 3.40] |  2.05 | 0.065"
      ))

  })
}