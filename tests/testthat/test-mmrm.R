if (requiet("mmrm") && packageVersion("insight") > "0.18.8") {
  data(fev_data)
  m1 <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data
  )

  test_that("model_parameters", {
    out1 <- coef(summary(m1))
    out2 <- model_parameters(m1)

    expect_equal(
      out1[, "Estimate"],
      out2$Coefficient,
      tolerance = 1e-4
    )
    expect_identical(
      rownames(out1),
      out2$Parameter
    )
    expect_equal(
      out1[, "df"],
      out2$df_error,
      tolerance = 1e-4
    )
    expect_equal(
      out1[, "Pr(>|t|)"],
      out2$p,
      tolerance = 1e-4
    )
    expect_equal(
      out1[, "t value"],
      out2$t,
      tolerance = 1e-4
    )
    expect_equal(
      out1[, "Std. Error"],
      out2$SE,
      tolerance = 1e-4
    )
    expect_identical(attributes(out2)$ci_method, "ci_method")
  })

  m1 <- mmrm(
    formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
    data = fev_data,
    method = "Kenward-Roger"
  )

  test_that("model_parameters", {
    out1 <- coef(summary(m1))
    out2 <- model_parameters(m1)

    expect_equal(
      out1[, "Estimate"],
      out2$Coefficient,
      tolerance = 1e-4
    )
    expect_identical(
      rownames(out1),
      out2$Parameter
    )
    expect_equal(
      out1[, "df"],
      out2$df_error,
      tolerance = 1e-4
    )
    expect_equal(
      out1[, "Pr(>|t|)"],
      out2$p,
      tolerance = 1e-4
    )
    expect_equal(
      out1[, "t value"],
      out2$t,
      tolerance = 1e-4
    )
    expect_equal(
      out1[, "Std. Error"],
      out2$SE,
      tolerance = 1e-4
    )
  })
  expect_identical(attributes(out2)$ci_method, "ci_method")
}
