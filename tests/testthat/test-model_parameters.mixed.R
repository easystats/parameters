.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  require("testthat") &&
  require("parameters") &&
  require("lme4")) {
  data(mtcars)
  m1 <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  m2 <- lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")

  test_that("model_parameters.mixed", {
    params <- model_parameters(m1)
    expect_equal(c(nrow(params), ncol(params)), c(2, 9))
    expect_equal(params$CI_high, c(1.6373105660317, 0.554067677205595), tolerance = 1e-3)

    params <- model_parameters(m1, ci = c(0.8, 0.9))
    expect_equal(c(nrow(params), ncol(params)), c(2, 10))
    expect_equal(params$CI_high_0.8, c(1.29595665381331, 0.502185700948862), tolerance = 1e-3)
    expect_equal(params$CI_high_0.9, c(1.47875781798108, 0.529969433080186), tolerance = 1e-3)

    params <- model_parameters(m2)
    expect_equal(c(nrow(params), ncol(params)), c(2, 9))

    model <- lme4::glmer(vs ~ drat + cyl + (1 | gear), data = mtcars, family = "binomial")
    params <- model_parameters(model)
    cs <- coef(summary(model))
    expect_equal(c(nrow(params), ncol(params)), c(3, 9))
    expect_equal(params$Parameter, rownames(cs))

    # TODO: Not sure how to deal with bootstrapped mixed models... As it throws an unreasonable amount of singular fits...
  })


  test_that("model_parameters.mixed-random", {
    params <- model_parameters(m1, effects = "random")
    expect_equal(c(nrow(params), ncol(params)), c(3, 9))
    expect_equal(as.vector(params$Parameter), c("(Intercept)", "(Intercept)", "(Intercept)"))
    expect_equal(as.vector(params$Level), c("3", "4", "5"))
    expect_equal(params$Coefficient, c(0.1692, 0.0566, -0.2259), tolerance = 1e-2)
  })

  test_that("model_parameters.mixed-ran_pars", {
    params <- model_parameters(m1, effects = "ran_pars")
    expect_equal(c(nrow(params), ncol(params)), c(2, 9))
    expect_equal(
      as.vector(params$Parameter),
      c("SD (Observations)", "SD (Intercept)")
    )
    expect_equal(as.vector(params$Level), c(NA, NA))
    expect_equal(params$Coefficient, c(0.59385, 0.27049), tolerance = 1e-2)
  })

  test_that("model_parameters.mixed-all", {
    params <- model_parameters(m1, effects = "all")
    expect_equal(c(nrow(params), ncol(params)), c(7, 9))
    expect_equal(
      as.vector(params$Parameter),
      c("(Intercept)", "cyl", "(Intercept)", "(Intercept)", "(Intercept)",
        "SD (Observations)", "SD (Intercept)")
    )
    expect_equal(as.vector(params$Level), c(NA, NA, "3", "4", "5", NA, NA))
    expect_equal(params$Coefficient, c(0.65112, 0.40418, 0.16923, 0.05663, -0.22586, 0.59385, 0.27049), tolerance = 1e-2)
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
  mp <- model_parameters(model)

  test_that("model_parameters.mixed", {
    expect_equal(mp$Component, c("rewb-contextual", "rewb-contextual", "within", "between"))
  })
}
