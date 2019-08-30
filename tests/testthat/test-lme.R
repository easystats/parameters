if (require("testthat") &&
  require("parameters") &&
  require("nlme") &&
  require("lme4")) {

  data(Orthodont)
  m1 <- lme(Reaction ~ Days,
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
}
