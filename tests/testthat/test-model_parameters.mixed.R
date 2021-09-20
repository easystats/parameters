.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  requiet("testthat") &&
  requiet("parameters") &&
  requiet("lme4")) {
  data(mtcars)
  m1 <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  m2 <- lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")

  test_that("model_parameters.mixed", {
    params <- model_parameters(m1, df_method = "wald", effects = "fixed")
    expect_equal(c(nrow(params), ncol(params)), c(2, 10))
    expect_equal(params$CI_high, c(1.6373105660317, 0.554067677205595), tolerance = 1e-3)

    params <- model_parameters(m1, effects = "fixed")
    expect_equal(c(nrow(params), ncol(params)), c(2, 10))
    expect_equal(params$CI_high, c(1.68181, 0.56083), tolerance = 1e-3)

    params <- model_parameters(m1, ci = c(0.8, 0.9), df_method = "wald", effects = "fixed")
    expect_equal(c(nrow(params), ncol(params)), c(2, 12))
    expect_equal(params$CI_high_0.8, c(1.29595665381331, 0.502185700948862), tolerance = 1e-3)
    expect_equal(params$CI_high_0.9, c(1.47875781798108, 0.529969433080186), tolerance = 1e-3)

    params <- model_parameters(m1, ci = c(0.8, 0.9), effects = "fixed")
    expect_equal(c(nrow(params), ncol(params)), c(2, 12))
    expect_equal(params$CI_high_0.8, c(1.31154, 0.50455), tolerance = 1e-3)
    expect_equal(params$CI_high_0.9, c(1.50707, 0.53427), tolerance = 1e-3)

    params <- model_parameters(m2, effects = "fixed")
    expect_equal(c(nrow(params), ncol(params)), c(2, 10))

    model <- lme4::glmer(vs ~ drat + cyl + (1 | gear), data = mtcars, family = "binomial")
    params <- model_parameters(model, effects = "fixed")
    cs <- coef(summary(model))
    expect_equal(c(nrow(params), ncol(params)), c(3, 10))
    expect_equal(colnames(params), c(
      "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
      "z", "df_error", "p", "Effects"
    ))
    expect_equal(params$Parameter, rownames(cs))

    # TODO: Not sure how to deal with bootstrapped mixed models... As it throws an unreasonable amount of singular fits...
  })


  test_that("model_parameters.mixed-random", {
    params <- model_parameters(m1, effects = "random", group_level = TRUE)
    expect_equal(c(nrow(params), ncol(params)), c(3, 9))
    expect_equal(as.vector(params$Parameter), c("(Intercept)", "(Intercept)", "(Intercept)"))
    expect_equal(as.vector(params$Level), c("3", "4", "5"))
    expect_equal(params$Coefficient, c(0.1692, 0.0566, -0.2259), tolerance = 1e-2)
  })

  test_that("model_parameters.mixed-ran_pars", {
    params <- model_parameters(m1, effects = "random")
    expect_equal(c(nrow(params), ncol(params)), c(2, 8))
    expect_equal(
      as.vector(params$Parameter),
      c("SD (Intercept)", "SD (Observations)")
    )
    expect_equal(params$Coefficient, c(0.27049, 0.59385), tolerance = 1e-2)
  })

  test_that("model_parameters.mixed-all", {
    params <- model_parameters(m1, effects = "all")
    expect_equal(c(nrow(params), ncol(params)), c(4, 11))
    expect_equal(
      as.vector(params$Parameter),
      c("(Intercept)", "cyl", "SD (Intercept)", "SD (Observations)")
    )
    expect_equal(params$Coefficient, c(0.65112, 0.40418, 0.27049, 0.59385), tolerance = 1e-2)
  })

  test_that("model_parameters.mixed-all_pars", {
    params <- model_parameters(m1, effects = "all", group_level = TRUE)
    expect_equal(c(nrow(params), ncol(params)), c(5, 12))
    expect_equal(
      as.vector(params$Parameter),
      c("(Intercept)", "cyl", "(Intercept)", "(Intercept)", "(Intercept)")
    )
    expect_equal(as.vector(params$Level), c(NA, NA, "3", "4", "5"))
    expect_equal(params$Coefficient, c(0.65112, 0.40418, 0.16923, 0.05663, -0.22586), tolerance = 1e-2)
  })


  data("qol_cancer")
  qol_cancer <- cbind(
    qol_cancer,
    demean(qol_cancer, select = c("phq4", "QoL"), group = "ID")
  )
  model <- lmer(
    QoL ~ time + phq4_within + phq4_between + (1 | ID),
    data = qol_cancer
  )
  mp <- model_parameters(model, effects = "fixed")

  test_that("model_parameters.mixed", {
    expect_equal(mp$Component, c("rewb-contextual", "rewb-contextual", "within", "between"))
  })


  test_that("print-model_parameters", {
    out <- utils::capture.output(print(model_parameters(model, effects = "fixed")))
    expect_equal(
      out,
      c(
        "Parameter   | Coefficient |   SE |         95% CI | t(558) |      p",
        "-------------------------------------------------------------------",
        "(Intercept) |       71.53 | 1.56 | [68.48, 74.59] |  45.98 | < .001",
        "time        |        1.09 | 0.64 | [-0.17,  2.34] |   1.70 | 0.089 ",
        "",
        "# Within-Effects",
        "",
        "Parameter   | Coefficient |   SE |         95% CI | t(558) |      p",
        "-------------------------------------------------------------------",
        "phq4 within |       -3.66 | 0.41 | [-4.46, -2.86] |  -8.95 | < .001",
        "",
        "# Between-Effects",
        "",
        "Parameter    | Coefficient |   SE |         95% CI | t(558) |      p",
        "--------------------------------------------------------------------",
        "phq4 between |       -6.28 | 0.50 | [-7.27, -5.30] | -12.53 | < .001"
      )
    )
  })

  test_that("print-model_parameters", {
    out <- utils::capture.output(print(model_parameters(m1, effects = "all")))
    expect_equal(
      out,
      c(
        "# Fixed Effects",
        "",
        "Parameter   | Coefficient |   SE |        95% CI | t(28) |      p",
        "-----------------------------------------------------------------",
        "(Intercept) |        0.65 | 0.50 | [-0.38, 1.68] |  1.29 | 0.206 ",
        "cyl         |        0.40 | 0.08 | [ 0.25, 0.56] |  5.29 | < .001",
        "",
        "# Random Effects",
        "",
        "Parameter            | Coefficient",
        "----------------------------------",
        "SD (Intercept: gear) |        0.27",
        "SD (Residual)        |        0.59"
      )
    )

    out <- utils::capture.output(print(model_parameters(m1, effects = "fixed", summary = TRUE)))
    expect_equal(
      out,
      c(
        "# Fixed Effects",
        "",
        "Parameter   | Coefficient |   SE |        95% CI | t(28) |      p",
        "-----------------------------------------------------------------",
        "(Intercept) |        0.65 | 0.50 | [-0.38, 1.68] |  1.29 | 0.206 ",
        "cyl         |        0.40 | 0.08 | [ 0.25, 0.56] |  5.29 | < .001",
        "",
        "Model: wt ~ cyl (32 Observations)",
        "Residual standard deviation: 0.594 (df = 28)",
        "Conditional R2: 0.628; Marginal R2: 0.550"
      )
    )
  })
}
