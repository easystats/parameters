skip_on_cran()

m <- glm(am ~ mpg + hp + factor(cyl),
  data = mtcars, family = binomial()
)

test_that("model_parameters.anova", {
  a <- anova(m, test = "Chisq")
  mp <- model_parameters(a)

  expect_named(mp, c("Parameter", "df", "Deviance", "df_error", "Deviance_error", "p"))
  expect_equal(mp$Deviance_error, c(43.22973, 29.67517, 19.23255, 10.48692), tolerance = 1e-3)
  expect_equal(mp$p, c(NA, 0.00023, 0.00123, 0.01262), tolerance = 1e-3)
  expect_snapshot(mp)
})

test_that("model_parameters.anova", {
  skip_if_not_installed("car")
  a <- car::Anova(m, type = 3, test.statistic = "F")
  mp <- model_parameters(a)
  expect_named(mp, c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p"))
  expect_equal(mp[["F"]], c(53.40138, 60.42944, 13.96887, NA), tolerance = 1e-3)
})

test_that("model_parameters.anova for mixed models", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("lmerTest")
  m <- lmerTest::lmer(mpg ~ wt + (1 | gear), data = mtcars)
  out <- parameters::model_parameters(anova(m))
  expect_named(out, c("Parameter", "Sum_Squares", "df", "df_error", "Mean_Square", "F", "p"))
  expect_equal(out$df_error, 21.92272, tolerance = 1e-4)
})

test_that("linear hypothesis tests", {
  skip_if_not_installed("car")
  skip_if_not_installed("carData")
  data(Davis, package = "carData")
  data(Duncan, package = "carData")

  mod.davis <- lm(weight ~ repwt, data = Davis)

  ## the following are equivalent:
  p1 <- parameters(car::linearHypothesis(mod.davis, diag(2), c(0, 1)))
  p2 <- parameters(car::linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1")))
  p3 <- parameters(car::linearHypothesis(mod.davis, c("(Intercept)", "repwt"), c(0, 1)))
  p4 <- parameters(car::linearHypothesis(mod.davis, c("(Intercept)", "repwt = 1")))
  expect_equal(p1, p2, ignore_attr = TRUE)
  expect_equal(p1, p3, ignore_attr = TRUE)
  expect_equal(p1, p4, ignore_attr = TRUE)
  expect_identical(nrow(p1), 2L)
  expect_identical(p1$Parameter, c("(Intercept) = 0", "repwt = 1"))

  mod.duncan <- lm(prestige ~ income + education, data = Duncan)
  p <- parameters(car::linearHypothesis(mod.duncan, "1*income - 1*education + 1 = 1"))
  expect_identical(nrow(p), 1L)
  expect_identical(p$Parameter, "income - education = 0")
})

test_that("print-model_parameters", {
  skip_if_not_installed("car")
  a <- car::Anova(m, type = 3, test.statistic = "F")
  mp <- model_parameters(a)
  expect_snapshot(mp)
})


test_that("model_parameters_Anova.mlm", {
  skip_if_not_installed("car")

  m <- lm(cbind(hp, mpg) ~ factor(cyl) * am, data = mtcars)
  a <- car::Anova(m, type = 3, test.statistic = "Pillai")
  mp <- model_parameters(a, verbose = FALSE)

  expect_named(mp, c("Parameter", "df", "Statistic", "df_num", "df_error", "F", "p"))
  expect_equal(mp[["F"]], c(158.2578, 6.60593, 3.71327, 3.28975), tolerance = 1e-3)
  expect_equal(mp$Statistic, c(0.9268, 0.67387, 0.22903, 0.4039), tolerance = 1e-3)
})


test_that("model_parameters_Anova.mlm", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("car")
  data(housing, package = "MASS")
  m <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  a <- car::Anova(m)
  mp <- model_parameters(a)
  expect_named(mp, c("Parameter", "Chi2", "df", "p"))
  expect_equal(mp$Chi2, c(108.2392, 55.91008, 14.30621), tolerance = 1e-3)
})



test_that("model_parameters_Anova-effectsize", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("effectsize", minimum_version = "0.4.3")

  df <- iris
  df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

  mm <- suppressMessages(lme4::lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species), data = df))
  model <- anova(mm)

  # parameters table including effect sizes
  mp <- model_parameters(
    model,
    es_type = "eta",
    ci = 0.9,
    df_error = dof_satterthwaite(mm)[2:3]
  )

  expect_identical(
    colnames(mp),
    c(
      "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "Eta2_partial",
      "Eta2_CI_low", "Eta2_CI_high"
    )
  )
  expect_equal(mp$Eta2_partial, c(0.03262, 0.6778), tolerance = 1e-3)
})


# XXX -----

test_that("anova type | lm", {
  skip_if_not_installed("car")

  m <- lm(mpg ~ factor(cyl) * hp + disp, mtcars)

  a1 <- aov(m)
  expect_identical(attr(model_parameters(a1), "anova_type"), 1)

  a1 <- anova(m)
  expect_identical(attr(model_parameters(a1), "anova_type"), 1)

  a2 <- car::Anova(m, type = 2)
  a3 <- car::Anova(m, type = 3)
  expect_identical(attr(model_parameters(a2), "anova_type"), 2)
  expect_message(
    expect_identical(attr(model_parameters(a3), "anova_type"), 3),
    "Type 3 ANOVAs only give"
  )

  m <- lm(mpg ~ factor(cyl) + hp + disp, mtcars)
  expect_warning(model_parameters(aov(m)), regexp = NA) # no need for warning, because no interactions

  m <- lm(mpg ~ factor(cyl) * scale(disp, TRUE, FALSE) + scale(disp, TRUE, FALSE),
    mtcars,
    contrasts = list("factor(cyl)" = contr.helmert)
  )
  a3 <- car::Anova(m, type = 3)
  expect_message(
    model_parameters(a3),
    "Type 3 ANOVAs only give"
  )
})

test_that("anova type | mlm", {
  skip_if_not_installed("car")

  m <- lm(cbind(mpg, drat) ~ factor(cyl) * hp + disp, mtcars)

  a1 <- aov(m)
  expect_identical(attr(model_parameters(a1), "anova_type"), 1)

  a1 <- anova(m)
  expect_identical(attr(model_parameters(a1), "anova_type"), 1)

  a2 <- car::Anova(m, type = 2)
  a3 <- car::Anova(m, type = 3)
  expect_identical(attr(model_parameters(a2), "anova_type"), 2)
  expect_identical(attr(model_parameters(a3, verbose = FALSE), "anova_type"), 3)
})

test_that("anova type | glm", {
  skip_if_not_installed("car")

  m <- suppressWarnings(glm(am ~ factor(cyl) * hp + disp, mtcars, family = binomial()))

  a1 <- anova(m)
  expect_identical(attr(model_parameters(a1), "anova_type"), 1)

  a2 <- suppressWarnings(car::Anova(m, type = 2))
  a3 <- suppressWarnings(car::Anova(m, type = 3))
  expect_identical(attr(model_parameters(a2), "anova_type"), 2)
  expect_message(
    expect_identical(attr(model_parameters(a3), "anova_type"), 3),
    "Type 3 ANOVAs only give"
  )
})

test_that("anova type | lme4", {
  skip_if_not_installed("lmerTest")
  skip_if_not_installed("lme4")
  skip_if_not_installed("car")

  m1 <- lme4::lmer(mpg ~ factor(cyl) * hp + disp + (1 | gear), mtcars)
  suppressMessages({
    m2 <- lme4::glmer(carb ~ factor(cyl) * hp + disp + (1 | gear), mtcars,
      family = poisson()
    )
  })

  a1 <- anova(m1)
  expect_identical(attr(model_parameters(a1), "anova_type"), 1)

  a1 <- anova(m2)
  expect_identical(attr(model_parameters(a1), "anova_type"), 1)

  a3 <- anova(lmerTest::as_lmerModLmerTest(m1))
  expect_message(
    expect_identical(attr(model_parameters(a3), "anova_type"), 3),
    "Type 3 ANOVAs only give"
  )

  a2 <- car::Anova(m1, type = 2)
  a3 <- car::Anova(m1, type = 3)
  expect_identical(attr(model_parameters(a2), "anova_type"), 2)
  expect_message(
    expect_identical(attr(model_parameters(a3), "anova_type"), 3),
    "Type 3 ANOVAs only give"
  )

  a2 <- car::Anova(m2, type = 2)
  a3 <- car::Anova(m2, type = 3)
  expect_identical(attr(model_parameters(a2), "anova_type"), 2)
  expect_message(
    expect_identical(attr(model_parameters(a3), "anova_type"), 3),
    "Type 3 ANOVAs only give"
  )
})

test_that("anova type | afex + Anova.mlm", {
  skip_if_not_installed("afex")

  data(obk.long, package = "afex")

  suppressMessages({
    m <- afex::aov_ez("id", "value", obk.long,
      between = c("treatment", "gender"),
      within = c("phase", "hour"), observed = "gender"
    )
  })

  expect_identical(attr(model_parameters(m), "anova_type"), 3)
  expect_identical(attr(model_parameters(m$Anova, verbose = FALSE), "anova_type"), 3)
})

test_that("anova rms", {
  skip_if_not_installed("rms")
  m <- rms::ols(mpg ~ cyl + disp + hp + drat, data = mtcars)
  a <- anova(m)
  mp <- model_parameters(a)

  expect_identical(attr(mp, "anova_type"), 2)
  expect_identical(mp$Parameter, c("cyl", "disp", "hp", "drat", "Total", "Residuals"))
  expect_identical(colnames(mp), c("Parameter", "Sum_Squares_Partial", "df", "Mean_Square", "F", "p"))
  expect_equal(mp$Sum_Squares_Partial, data.frame(a)$Partial.SS, tolerance = 1e-3)
})

test_that("anova rms", {
  skip_if_not_installed("rms")
  skip_if(getRversion() < "4.2.0")
  m <- rms::orm(mpg ~ cyl + disp + hp + drat, data = mtcars)
  a <- anova(m)
  mp <- model_parameters(a)

  expect_identical(attr(mp, "anova_type"), 2)
  expect_identical(mp$Parameter, c("cyl", "disp", "hp", "drat", "Total"))
  expect_named(mp, c("Parameter", "Chi2", "df", "p"))
  expect_equal(mp$Chi2, data.frame(a)$Chi.Square, tolerance = 1e-3)
})
