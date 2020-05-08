if (require("testthat") &&
  require("parameters") &&
  require("nlme") &&
  require("lme4") &&
  require("insight")) {

  if (packageVersion("insight") > "0.8.3") {
    data("sleepstudy")
    m1 <- nlme::lme(Reaction ~ Days,
                    random = ~ 1 + Days | Subject,
                    data = sleepstudy
    )

    test_that("ci", {
      expect_equal(
        ci(m1)$CI_low,
        c(237.927995380985, 7.4146616764556),
        tolerance = 1e-4
      )
    })

    test_that("se", {
      expect_equal(
        standard_error(m1)$SE,
        c(6.82451602451407, 1.54578275017725),
        tolerance = 1e-4
      )
    })

    test_that("p_value", {
      expect_equal(
        p_value(m1)$p,
        c(2.38350215912719e-80, 2.26328050057813e-10),
        tolerance = 1e-4
      )
    })

    test_that("model_parameters", {
      expect_equal(
        model_parameters(m1)$Coefficient,
        c(251.405104848485, 10.467285959596),
        tolerance = 1e-4
      )
    })

    data("Orthodont")
    m2 <- nlme::lme(
      distance ~ age + Sex,
      random =  ~ 1 | Subject,
      data = Orthodont,
      method = "ML"
    )

    test_that("model_parameters", {
      params <- model_parameters(m2)
      expect_equal(params$Coefficient, c(17.70671, 0.66019, -2.32102), tolerance = 1e-4)
      expect_equal(params$SE, c(0.83155, 0.06209, 0.74307), tolerance = 1e-4)
      # expect_equal(params$df, c(80, 80, 25), tolerance = 1e-4)
      expect_equal(params$CI_low, c(16.07503, 0.53834, -3.82999), tolerance = 1e-4)
    })

    test_that("model_parameters, satterthwaite", {
      params <- model_parameters(m2, df_method = "satterthwaite")
      expect_equal(params$Coefficient, c(17.70671, 0.66019, -2.32102), tolerance = 1e-4)
      expect_equal(params$SE, c(0.83155, 0.06209, 0.74307), tolerance = 1e-4)
      # expect_equal(params$df, c(104.1503, 82.87867, 26.25), tolerance = 1e-4)
      expect_equal(params$CI_low, c(16.05848, 0.5379, -3.81337), tolerance = 1e-4)
    })
  }
}
