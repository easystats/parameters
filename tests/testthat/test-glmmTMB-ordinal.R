skip_on_cran()
skip_if_not_installed("glmmTMB", minimum_version = "1.1.15")
skip_if_not_installed("insight", minimum_version = "1.6.0")
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
})
