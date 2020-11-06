if (require("testthat") &&
  require("parameters") &&
  require("lme4")) {
  data(mtcars)
  m1 <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
  m2 <- lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")

  test_that("model_parameters.mixed", {
    params <- model_parameters(m1)
    testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))
    testthat::expect_equal(params$CI_high, c(1.6373105660317, 0.554067677205595), tolerance = 1e-3)

    params <- model_parameters(m1, ci = c(0.8, 0.9))
    testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 10))
    testthat::expect_equal(params$CI_high_80, c(1.29595665381331, 0.502185700948862), tolerance = 1e-3)
    testthat::expect_equal(params$CI_high_90, c(1.47875781798108, 0.529969433080186), tolerance = 1e-3)

    params <- model_parameters(m2)
    testthat::expect_equal(c(nrow(params), ncol(params)), c(2, 8))

    model <- lme4::glmer(vs ~ drat + cyl + (1 | gear), data = mtcars, family = "binomial")
    params <- model_parameters(model)
    cs <- coef(summary(model))
    testthat::expect_equal(c(nrow(params), ncol(params)), c(3, 8))
    testthat::expect_equal(params$Parameter, rownames(cs))

    # TODO: Not sure how to deal with bootstrapped mixed models... As it throws an unreasonable amount of singular fits...
  })
}
