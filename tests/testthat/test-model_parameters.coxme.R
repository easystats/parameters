skip_on_cran()
skip_if_not_installed("coxme")
skip_if_not_installed("survival")
skip_if_not_installed("withr")

# modelparameters ----------------------------------

withr::with_environment(
  new.env(),
  test_that("model_parameters.coxme", {
    Surv <- survival::Surv
    rats <- survival::rats
    lung <- survival::lung

    set.seed(1234)
    rats$grp <- sample(letters[1:3], nrow(rats), replace = TRUE)

    data(eortc, package = "coxme")
    d <- coxme::eortc
    d2 <<- rats

    m1 <- coxme::coxme(Surv(y, uncens) ~ trt + (1 | center), data = d)
    out <- model_parameters(m1)
    expect_named(
      out,
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "z", "df_error", "p", "Effects", "Group")
    )
    expect_equal(out$Coefficient, c(0.708613, 0.329214, NA), tolerance = 1e-4)

    m2 <- coxme::coxme(Surv(time, status) ~ ph.ecog + age + (1 | inst), lung)
    out <- model_parameters(m2)
    expect_named(
      out,
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "z", "df_error", "p", "Effects", "Group")
    )
    expect_equal(out$Coefficient, c(0.473195, 0.011394, 0.146955, NA), tolerance = 1e-4)

    m3 <- coxme::coxme(Surv(time, status) ~ rx + (1 + rx | litter) + (1 | grp), d2)
    out <- model_parameters(m3)
    expect_named(
      out,
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "z", "df_error", "p", "Effects", "Group")
    )
    expect_equal(
      out$Coefficient,
      c(0.730075, 1.147669, 0.018608, 0.038953, 0.000791, NA),
      tolerance = 1e-4
    )
    expect_identical(
      out$Parameter,
      c("rx", "SD (Intercept)", "SD (Intercept)", "SD (rx)", "Cor (Intercept~rx)", "SD (Observations)")
    )

    out <- model_parameters(m3, effects = "fixed")
    expect_equal(out$Coefficient, 0.730075, tolerance = 1e-4)
    expect_identical(out$Parameter, "rx")

    out <- model_parameters(m3, effects = "random")
    expect_equal(
      out$Coefficient,
      c(1.147669, 0.018608, 0.038953, 0.000791, NA),
      tolerance = 1e-4
    )
    expect_identical(
      out$Parameter,
      c("SD (Intercept)", "SD (Intercept)", "SD (rx)", "Cor (Intercept~rx)", "SD (Observations)")
    )
  })
)
