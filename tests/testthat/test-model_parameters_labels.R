skip_if_not_installed("withr")

withr::with_options(
  list(parameters_interaction = "*"),
  {
    test_that("model_parameters_labels", {
      skip_if_not_installed("lme4")
      skip_if_not_installed("merDeriv")
      data(mtcars)
      mtcars$am <- as.factor(mtcars$am)

      m1 <- lme4::lmer(mpg ~ hp * am + (1 | cyl), data = mtcars)
      expect_equal(
        attr(model_parameters(m1), "pretty_names"),
        c(`(Intercept)` = "(Intercept)", hp = "hp", am1 = "am [1]", `hp:am1` = "hp * am [1]")
      )

      m2 <- lme4::lmer(mpg ~ hp * as.factor(am) + (1 | cyl), data = mtcars)
      expect_equal(
        attr(model_parameters(m2), "pretty_names"),
        c(
          `(Intercept)` = "(Intercept)", hp = "hp", `as.factor(am)1` = "am [1]",
          `hp:as.factor(am)1` = "hp * am [1]"
        )
      )

      m3 <- lme4::lmer(mpg ~ hp * log(gear) + (1 | cyl), data = mtcars)
      expect_equal(
        attr(model_parameters(m3), "pretty_names"),
        c(
          `(Intercept)` = "(Intercept)", hp = "hp", `log(gear)` = "gear [log]",
          `hp:log(gear)` = "hp * gear [log]"
        )
      )

      m4 <- lm(mpg ~ as.factor(cyl) + hp * log(gear), data = mtcars)
      expect_equal(
        attr(model_parameters(m4), "pretty_names"),
        c(
          `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl [6]",
          `as.factor(cyl)8` = "cyl [8]", hp = "hp", `log(gear)` = "gear [log]",
          `hp:log(gear)` = "hp * gear [log]"
        )
      )

      m5 <- lm(mpg ~ as.factor(cyl) * I(wt / 10) + hp * log(gear), data = mtcars)
      expect_equal(
        attr(model_parameters(m5), "pretty_names"),
        c(
          `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl [6]",
          `as.factor(cyl)8` = "cyl [8]", `I(wt/10)` = "wt/10", hp = "hp",
          `log(gear)` = "gear [log]", `as.factor(cyl)6:I(wt/10)` = "cyl [6] * wt/10",
          `as.factor(cyl)8:I(wt/10)` = "cyl [8] * wt/10", `hp:log(gear)` = "hp * gear [log]"
        )
      )

      m6 <- lm(mpg ~ as.factor(cyl) * log(wt) + hp * log(gear), data = mtcars)
      expect_equal(
        attr(model_parameters(m6), "pretty_names"),
        c(
          `(Intercept)` = "(Intercept)", `as.factor(cyl)6` = "cyl [6]",
          `as.factor(cyl)8` = "cyl [8]", `log(wt)` = "wt [log]", hp = "hp",
          `log(gear)` = "gear [log]", `as.factor(cyl)6:log(wt)` = "cyl [6] * wt [log]",
          `as.factor(cyl)8:log(wt)` = "cyl [8] * wt [log]", `hp:log(gear)` = "hp * gear [log]"
        )
      )

      m7 <- lm(mpg ~ as.factor(cyl) * poly(wt, 2) + hp * log(gear), data = mtcars)
      expect_equal(
        attr(model_parameters(m7), "pretty_names"),
        c(
          `(Intercept)` = "(Intercept)",
          `as.factor(cyl)6` = "cyl6",
          `as.factor(cyl)8` = "cyl8",
          `poly(wt, 2)1` = "wt [1st degree]",
          `poly(wt, 2)2` = "wt [2nd degree]",
          hp = "hp",
          `log(gear)` = "gear [log]",
          `as.factor(cyl)6:poly(wt, 2)1` = "cyl6 * wt [1st degree]",
          `as.factor(cyl)8:poly(wt, 2)1` = "cyl8 * wt [1st degree]",
          `as.factor(cyl)6:poly(wt, 2)2` = "cyl6 * wt [2nd degree]",
          `as.factor(cyl)8:poly(wt, 2)2` = "cyl8 * wt [2nd degree]",
          `hp:log(gear)` = "hp * gear [log]"
        )
      )

      m8 <- lm(mpg ~ as.factor(cyl) * I(wt^2) + hp * log(gear), data = mtcars)
      expect_equal(
        attr(model_parameters(m8), "pretty_names"),
        c(
          `(Intercept)` = "(Intercept)",
          `as.factor(cyl)6` = "cyl [6]",
          `as.factor(cyl)8` = "cyl [8]",
          `I(wt^2)` = "wt^2",
          hp = "hp",
          `log(gear)` = "gear [log]",
          `as.factor(cyl)6:I(wt^2)` = "cyl [6] * wt^2",
          `as.factor(cyl)8:I(wt^2)` = "cyl [8] * wt^2",
          `hp:log(gear)` = "hp * gear [log]"
        )
      )
    })

    test_that("Issue #785: partial and factor labels", {
      dat <- mtcars
      dat$cyl <- factor(dat$cyl)
      attr(dat$hp, "label") <- "Horsepower"
      attr(dat$cyl, "label") <- "Cylinders"
      m <- lm(mpg ~ hp + drat + cyl, data = dat)
      mp <- model_parameters(m)
      known <- c("(Intercept)", "Horsepower", "drat", "Cylinders [6]", "Cylinders [8]")
      expect_equal(attr(mp, "pretty_labels"), known, ignore_attr = TRUE)
    })

    test_that("Issue #806: Missing label for variance component in lme4", {
      skip_if_not_installed("lme4")
      skip_if_not_installed("merDeriv")
      mod <- lme4::lmer(mpg ~ hp + (1 | gear), data = mtcars)
      p <- parameters::parameters(mod, pretty_names = "labels")
      expect_true("SD (Intercept)" %in% attr(p, "pretty_labels"))
    })

    test_that("Issue #1142: character variable in model doesn't break other labels", {
      dat <- mtcars
      dat$gear <- as.character(dat$gear)
      attr(dat$hp, "label") <- "Horsepower"
      attr(dat$gear, "label") <- "Number of forward gears"

      # Including a character variable should not make hp's label disappear
      mod <- lm(mpg ~ hp + gear, data = dat)
      mp <- model_parameters(mod)
      known <- c(
        "(Intercept)", "Horsepower",
        "Number of forward gears [3]",
        "Number of forward gears [4]",
        "Number of forward gears [5]"
      )
      expect_equal(attr(mp, "pretty_labels"), known, ignore_attr = TRUE)
    })

    test_that("Issue #1135: on-the-fly factor with labelled data, no NA in interaction labels", {
      dat <- mtcars
      attr(dat$am, "label") <- "Transmission"
      attr(dat$cyl, "label") <- "Cylinders"

      # factor() on-the-fly conversion in formula
      mod <- lm(mpg ~ am * factor(cyl), data = dat)
      mp <- model_parameters(mod)
      labels <- attr(mp, "pretty_labels")

      # No NA should appear in interaction labels
      expect_false(any(grepl("\\bNA\\b", labels, ignore.case = FALSE)))
      # The am label should be used
      expect_true(any(grepl("Transmission", labels)))
      # Cylinders label should appear in the factor levels
      expect_true(any(grepl("Cylinders", labels)))
    })

    test_that("Issue #1130: include_reference works for pscl zeroinfl models", {
      skip_if_not_installed("pscl")
      dat <- pscl::bioChemists
      mod <- pscl::zeroinfl(art ~ fem + mar, data = dat)
      out <- model_parameters(mod, include_reference = TRUE)
      # Reference levels should be present in the count component
      expect_true(any(grepl("\\[Men\\]", out$Parameter)))
      expect_true(any(grepl("\\[Single\\]", out$Parameter)))
    })
  }
)

