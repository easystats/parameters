skip_if_not_installed("withr")

# make sure we have the correct interaction mark for tests
withr::with_options(
  list(parameters_interaction = "*"),
  {
    data(iris)
    m1 <- lm(Sepal.Length ~ Species, data = iris)
    m2 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
    counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
    outcome <- gl(3, 1, 9)
    treatment <- gl(3, 3)
    m3 <- glm(counts ~ outcome + treatment, family = poisson())

    x <- compare_parameters(m1, m2, m3)
    test_that("compare_parameters, default", {
      expect_identical(
        colnames(x),
        c(
          "Parameter", "Component", "Effects", "Coefficient.m1", "SE.m1", "CI.m1",
          "CI_low.m1", "CI_high.m1", "t.m1", "df_error.m1", "p.m1", "Coefficient.m2",
          "SE.m2", "CI.m2", "CI_low.m2", "CI_high.m2", "t.m2", "df_error.m2",
          "p.m2", "Log-Mean.m3", "SE.m3", "CI.m3", "CI_low.m3", "CI_high.m3",
          "z.m3", "df_error.m3", "p.m3"
        )
      )
      out <- capture.output(x)
      expect_length(out, 14)
      out <- format(x, select = "ci")
      expect_identical(colnames(out), c("Parameter", "m1", "m2", "m3"))
      expect_identical(
        out$Parameter,
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length", "outcome [2]", "outcome [3]",
          "treatment [2]", "treatment [3]", NA, "Observations"
        )
      )
    })


    x <- compare_parameters(m1, m2, m3, select = "se_p2")
    test_that("compare_parameters, se_p2", {
      expect_identical(
        colnames(x),
        c(
          "Parameter", "Component", "Effects", "Coefficient.m1", "SE.m1", "CI.m1",
          "CI_low.m1", "CI_high.m1", "t.m1", "df_error.m1", "p.m1", "Coefficient.m2",
          "SE.m2", "CI.m2", "CI_low.m2", "CI_high.m2", "t.m2", "df_error.m2",
          "p.m2", "Log-Mean.m3", "SE.m3", "CI.m3", "CI_low.m3", "CI_high.m3",
          "z.m3", "df_error.m3", "p.m3"
        )
      )
      out <- capture.output(x)
      expect_length(out, 14)
      out <- format(x, select = "se_p2")
      expect_identical(
        colnames(out),
        c(
          "Parameter", "Estimate (SE) (m1)", "p (m1)", "Estimate (SE) (m2)",
          "p (m2)", "Estimate (SE) (m3)", "p (m3)"
        )
      )
      expect_identical(
        out$Parameter,
        c(
          "(Intercept)", "Species [versicolor]", "Species [virginica]",
          "Petal Length", "Species [versicolor] * Petal Length",
          "Species [virginica] * Petal Length", "outcome [2]", "outcome [3]",
          "treatment [2]", "treatment [3]", NA, "Observations"
        )
      )
    })


    data(mtcars)
    m1 <- lm(mpg ~ wt, data = mtcars)
    m2 <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")

    test_that("compare_parameters, column name with escaping regex characters", {
      out <- utils::capture.output(compare_parameters(m1, m2, column_names = c("linear model (m1)", "logistic reg. (m2)")))
      expect_identical(out[1], "Parameter    |    linear model (m1) |   logistic reg. (m2)")
    })


    data(mtcars)
    m1 <- lm(mpg ~ hp, mtcars)
    m2 <- lm(mpg ~ hp, mtcars)
    test_that("compare_parameters, proper printing for CI=NULL #820", {
      expect_snapshot(compare_parameters(m1, m2, ci = NULL))
    })


    skip_on_cran()


    test_that("compare_parameters, correct random effects", {
      suppressWarnings(skip_if_not_installed("glmmTMB"))
      skip_if_not(getRversion() >= "4.0.0")

      data("fish")
      m0 <- glm(count ~ child + camper, data = fish, family = poisson())

      m1 <- glmmTMB::glmmTMB(
        count ~ child + camper + (1 | persons) + (1 | ID),
        data = fish,
        family = poisson()
      )

      m2 <- glmmTMB::glmmTMB(
        count ~ child + camper + zg + (1 | ID),
        ziformula = ~ child + (1 | persons),
        data = fish,
        family = glmmTMB::truncated_poisson()
      )

      cp <- compare_parameters(m0, m1, m2, effects = "all", component = "all")
      expect_snapshot(cp)
    })


    test_that("compare_parameters, print_md", {
      skip_if_not_installed("lme4")
      data(sleepstudy, package = "lme4")
      set.seed(1234)
      sleepstudy$grp <- as.factor(sample.int(3, nrow(sleepstudy), replace = TRUE))
      lm1 <- lme4::lmer(Reaction ~ Days + grp + (1 | Subject), data = sleepstudy)
      lm2 <- lme4::lmer(Reaction ~ Days * grp + (1 | Subject), data = sleepstudy)

      cp <- compare_parameters(lm1, lm2, select = "{estimate} ({ci})|{p}", drop = "^\\(Intercept")
      out <- print_md(cp, groups = list(
        Groups = c("grp (2)", "grp (3)"),
        Interactions = c("Days * grp (2)", "Days * grp (3)"),
        Controls = "Days"
      ))
      expect_snapshot(print(out))

      cp <- compare_parameters(lm1, lm2, select = "{estimate} ({ci})|{p}", drop = "^\\(Intercept", effects = "all")
      expect_snapshot(print_md(cp))

      # error
      expect_error(
        print_md(cp, groups = list(
          Groups = c("grp (2)", "grp (3)"),
          Interactions = c("Days * grp (2)", "Days * grp (3)"),
          Controls = "Days"
        )),
        regex = "Cannot combine"
      )

      # with reference level
      cp <- compare_parameters(lm1, lm2, select = "{estimate} ({ci})|{p}", drop = "^\\(Intercept", include_reference = TRUE)
      out <- print_md(cp, groups = list(
        Groups = 2:4,
        Interactions = 5:6,
        Controls = 1
      ))
      expect_snapshot(print(out))

      # with reference level
      cp <- compare_parameters(lm1, lm2, drop = "^\\(Intercept", include_reference = TRUE)
      out <- print_md(cp, groups = list(
        Groups = 2:4,
        Interactions = 5:6,
        Controls = 1
      ))
      expect_snapshot(print(out))

      # error
      cp <- compare_parameters(lm1, lm2, select = "{estimate} ({ci})|{p}", drop = "^\\(Intercept")
      expect_error(
        print_md(cp, groups = list(
          Groups = c("grp (2)", "grp (3)"),
          Interactions = c("Days * grp (2)", "Days * grp (3)"),
          Controls = "XDays"
        )),
        regex = "Some group indices"
      )
      expect_error(
        print_md(cp, groups = list(
          Groups = 1:2,
          Interactions = 4:5,
          Controls = 10
        )),
        regex = "Some group indices"
      )

      # output identical for both calls
      cp1 <- compare_parameters(lm1, lm2, select = "{estimate} ({ci})|{p}", drop = "^\\(Intercept")
      out1 <- capture.output(print_md(cp1, groups = list(
        Groups = c("grp (2)", "grp (3)"),
        Interactions = c("Days * grp (2)", "Days * grp (3)"),
        Controls = "Days"
      )))
      cp2 <- compare_parameters(
        lm1,
        lm2,
        select = "{estimate} ({ci})|{p}",
        drop = "^\\(Intercept",
        groups = list(
          Groups = c("grp (2)", "grp (3)"),
          Interactions = c("Days * grp (2)", "Days * grp (3)"),
          Controls = "Days"
        )
      )
      out2 <- capture.output(print_md(cp2))
      expect_identical(out1, out2)
    })
  }
)

skip_on_cran()
skip_if_not_installed("blme")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")

test_that("compare_parameters, works with blmer and glmmTMB", {
  data(sleepstudy, package = "lme4")
  control <- lme4::lmerControl(check.conv.grad = "ignore")
  fm1 <- blme::blmer(Reaction ~ Days + (0 + Days | Subject), sleepstudy,
    control = control,
    cov.prior = gamma
  )
  fm2 <- glmmTMB::glmmTMB(Reaction ~ Days + (1 + Days | Subject), sleepstudy)
  expect_silent(compare_parameters(fm1, fm2))
})
