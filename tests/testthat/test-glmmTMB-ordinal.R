skip_on_cran()
skip_if_not_installed("glmmTMB", minimum_version = "1.1.15")
skip_if_not_installed("insight", minimum_version = "1.5.4.1")
skip_if_not_installed("ordinal")
skip_if_not_installed("withr")

data(wine, package = "ordinal")

withr::with_options(list(parameters_exponentiate = FALSE), {
  m_tmb <- glmmTMB::glmmTMB(
    rating ~ temp + contact + (1 | judge),
    data = wine,
    family = glmmTMB::ordinal()
  )
  m_clmm <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)

  test_that("standard_error, glmmTMB ordinal", {
    out <- standard_error(m_tmb)
    expect_identical(
      out$Parameter,
      c("1|2", "2|3", "3|4", "4|5", "tempwarm", "contactyes")
    )
    expect_equal(
      out$SE,
      unname(sqrt(diag(vcov(m_clmm)))[out$Parameter]),
      tolerance = 1e-3
    )
    expect_identical(unique(out$Component), "conditional")
    expect_message(
      expect_null(standard_error(m_tmb, component = "zi")),
      "no zero-inflation"
    )
  })

  test_that("model_parameters, glmmTMB ordinal matches clmm", {
    out <- model_parameters(m_tmb)
    ref <- model_parameters(m_clmm)
    expect_identical(out$Parameter, ref$Parameter)
    for (col in c("Coefficient", "SE", "CI_low", "CI_high", "z", "p")) {
      expect_equal(out[[col]], ref[[col]], tolerance = 1e-2, ignore_attr = TRUE)
    }
    # random effects are still reported
    out <- model_parameters(m_tmb, effects = "all")
    expect_true("SD (Intercept)" %in% out$Parameter)
  })
})
