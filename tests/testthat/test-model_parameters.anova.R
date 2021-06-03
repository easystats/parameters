.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("insight") && require("testthat") && require("parameters")) {
  data(mtcars)
  m <- glm(am ~ mpg + hp + factor(cyl),
    data = mtcars, family = binomial()
  )

  a <- anova(m, test = "Chisq")
  mp <- model_parameters(a)

  test_that("model_parameters.anova", {
    expect_equal(colnames(mp), c("Parameter", "df", "Deviance", "df_error", "Deviance_error", "p"))
    expect_equal(mp$Deviance_error, c(43.22973, 29.67517, 19.23255, 10.48692), tolerance = 1e-3)
    expect_equal(mp$p, c(NA, 0.00023, 0.00123, 0.01262), tolerance = 1e-3)
  })

  test_that("print-model_parameters", {
    out <- utils::capture.output(print(mp))
    expect_equal(
      out,
      c(
        "Parameter   | df | Deviance | df (error) | Deviance (error) |      p",
        "--------------------------------------------------------------------",
        "NULL        |    |          |         31 |            43.23 |       ",
        "mpg         |  1 |    13.55 |         30 |            29.68 | < .001",
        "hp          |  1 |    10.44 |         29 |            19.23 | 0.001 ",
        "factor(cyl) |  2 |     8.75 |         27 |            10.49 | 0.013 ",
        "",
        "Anova Table (Type 1 tests)"
      )
    )
  })

  if (require("car")) {
    a <- car::Anova(m, type = 3, test.statistic = "F")
    mp <- model_parameters(a)

    test_that("model_parameters.anova", {
      expect_equal(colnames(mp), c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p"))
      expect_equal(mp[["F"]], c(53.40138, 60.42944, 13.96887, NA), tolerance = 1e-3)
    })

    test_that("print-model_parameters", {
      out <- utils::capture.output(print(mp))
      expect_equal(
        out,
        c(
          "Parameter   | Sum_Squares | df | Mean_Square |     F |      p",
          "-------------------------------------------------------------",
          "mpg         |       16.72 |  1 |       16.72 | 53.40 | < .001",
          "hp          |       18.92 |  1 |       18.92 | 60.43 | < .001",
          "factor(cyl) |        8.75 |  2 |        4.37 | 13.97 | < .001",
          "Residuals   |        8.45 | 27 |        0.31 |       |       ",
          "",
          "Anova Table (Type 3 tests)"
        )
      )
    })


    m <- lm(cbind(hp, mpg) ~ factor(cyl) * am, data = mtcars)
    a <- car::Anova(m, type = 3, test.statistic = "Pillai")
    mp <- model_parameters(a)

    test_that("model_parameters_Anova.mlm", {
      expect_equal(colnames(mp), c("Parameter", "df", "Statistic", "df_num", "df_error", "F", "p"))
      expect_equal(mp[["F"]], c(158.2578, 6.60593, 3.71327, 3.28975), tolerance = 1e-3)
      expect_equal(mp$Statistic, c(0.9268, 0.67387, 0.22903, 0.4039), tolerance = 1e-3)
    })


    if (require("MASS")) {
      data(housing)
      m <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
      a <- car::Anova(m)
      mp <- model_parameters(a)

      test_that("model_parameters_Anova.mlm", {
        expect_equal(colnames(mp), c("Parameter", "Chi2", "df", "p"))
        expect_equal(mp$Chi2, c(108.2392, 55.91008, 14.30621), tolerance = 1e-3)
      })
    }
  }

  if (require("lme4") && require("effectsize") && utils::packageVersion("effectsize") > "0.4.3") {
    data(iris)
    df <- iris
    df$Sepal.Big <- ifelse(df$Sepal.Width >= 3, "Yes", "No")

    mm <- suppressMessages(lmer(Sepal.Length ~ Sepal.Big + Petal.Width + (1 | Species), data = df))
    model <- anova(mm)

    # parameters table including effect sizes
    mp <- model_parameters(
      model,
      eta_squared = "partial",
      ci = .9,
      df_error = dof_satterthwaite(mm)[2:3]
    )

    test_that("model_parameters_Anova-effectsize", {
      expect_equal(
        colnames(mp),
        c(
          "Parameter", "Sum_Squares", "df", "Mean_Square", "F", "Eta2_partial",
          "Eta2_CI_low", "Eta2_CI_high"
        )
      )
      expect_equal(mp$Eta2_partial, c(0.03262, 0.6778), tolerance = 1e-3)
    })
  }
}


# XXX -----

if (.runThisTest && require("parameters") && require("testthat")) {
  test_that("anova type | lm", {
    m <- lm(mpg ~ factor(cyl) * hp + disp, mtcars)

    a1 <- aov(m)
    expect_equal(attr(model_parameters(a1), "anova_type"), 1)

    a1 <- anova(m)
    expect_equal(attr(model_parameters(a1), "anova_type"), 1)

    skip_if_not_installed("car")

    a2 <- car::Anova(m, type = 2)
    a3 <- car::Anova(m, type = 3)
    expect_equal(attr(model_parameters(a2), "anova_type"), 2)
    expect_equal(attr(model_parameters(a3), "anova_type"), 3)

    m <- lm(mpg ~ factor(cyl) + hp + disp, mtcars)
    expect_warning(model_parameters(aov(m)), regexp = NA) # no need for warning, because no interactions

    m <- lm(mpg ~ factor(cyl) * scale(disp, TRUE, FALSE) + scale(disp, TRUE, FALSE),
      mtcars,
      contrasts = list("factor(cyl)" = contr.helmert)
    )
    a3 <- car::Anova(m, type = 3)
    expect_warning(model_parameters(a3), regexp = NA) # expect no warning
  })

  test_that("anova type | mlm", {
    m <- lm(cbind(mpg, drat) ~ factor(cyl) * hp + disp, mtcars)

    a1 <- aov(m)
    expect_equal(attr(model_parameters(a1), "anova_type"), 1)

    a1 <- anova(m)
    expect_equal(attr(model_parameters(a1), "anova_type"), 1)

    skip_if_not_installed("car")
    a2 <- car::Anova(m, type = 2)
    a3 <- car::Anova(m, type = 3)
    expect_equal(attr(model_parameters(a2), "anova_type"), 2)
    expect_equal(attr(model_parameters(a3, verbose = FALSE), "anova_type"), 3)
  })

  test_that("anova type | glm", {
    m <- suppressWarnings(glm(am ~ factor(cyl) * hp + disp, mtcars, family = binomial()))

    a1 <- anova(m)
    expect_equal(attr(model_parameters(a1), "anova_type"), 1)

    skip_if_not_installed("car")
    a2 <- suppressWarnings(car::Anova(m, type = 2))
    a3 <- suppressWarnings(car::Anova(m, type = 3))
    expect_equal(attr(model_parameters(a2), "anova_type"), 2)
    expect_equal(attr(model_parameters(a3), "anova_type"), 3)
  })

  test_that("anova type | lme4", {
    skip_if_not_installed("lmerTest")
    m1 <- lme4::lmer(mpg ~ factor(cyl) * hp + disp + (1 | gear), mtcars)
    m2 <- lme4::glmer(carb ~ factor(cyl) * hp + disp + (1 | gear), mtcars,
      family = poisson()
    )

    a1 <- anova(m1)
    expect_equal(attr(model_parameters(a1), "anova_type"), 1)

    a1 <- anova(m2)
    expect_equal(attr(model_parameters(a1), "anova_type"), 1)

    a3 <- anova(lmerTest::as_lmerModLmerTest(m1))
    expect_equal(attr(model_parameters(a3), "anova_type"), 3)

    skip_if_not_installed("car")
    a2 <- car::Anova(m1, type = 2)
    a3 <- car::Anova(m1, type = 3)
    expect_equal(attr(model_parameters(a2), "anova_type"), 2)
    expect_equal(attr(model_parameters(a3), "anova_type"), 3)

    a2 <- car::Anova(m2, type = 2)
    a3 <- car::Anova(m2, type = 3)
    expect_equal(attr(model_parameters(a2), "anova_type"), 2)
    expect_equal(attr(model_parameters(a3), "anova_type"), 3)
  })

  test_that("anova type | afex + Anova.mlm", {
    skip_if_not_installed("afex")

    data(obk.long, package = "afex")

    m <- afex::aov_ez("id", "value", obk.long,
      between = c("treatment", "gender"),
      within = c("phase", "hour"), observed = "gender"
    )

    expect_equal(attr(model_parameters(m), "anova_type"), 3)
    expect_equal(attr(model_parameters(m$Anova, verbose = FALSE), "anova_type"), 3)
  })
}
