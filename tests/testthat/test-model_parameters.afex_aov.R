test_that("afex_aov", {
  skip_if_not_installed("afex")

  data(obk.long, package = "afex")
  m_between <- suppressMessages(suppressWarnings(
    afex::aov_car(value ~ treatment * gender + Error(id), data = obk.long)
  ))
  m_within <- suppressMessages(suppressWarnings(
    afex::aov_car(value ~ Error(id / (phase * hour)), data = obk.long)
  ))

  mp1 <- model_parameters(m_between, verbose = FALSE)
  mp2 <- model_parameters(m_within, verbose = FALSE)

  expect_equal(c(nrow(mp1), ncol(mp1)), c(4, 7))
  expect_equal(mp1$Sum_Squares, c(11.98202, 5.56322, 8.68275, 15.2037), tolerance = 1e-3)
  expect_equal(c(nrow(mp2), ncol(mp2)), c(3, 9))
  expect_equal(mp2$Sum_Squares, c(167.5, 106.29167, 11.08333), tolerance = 1e-3)
  expect_named(
    mp1,
    c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p", "Method")
  )
  expect_named(
    mp2,
    c(
      "Parameter", "Sum_Squares", "Sum_Squares_Error", "df", "df_error",
      "Mean_Square", "F", "p", "Method"
    )
  )

  # include intercept
  out <- model_parameters(m_between, verbose = FALSE, include_intercept = TRUE)
  expect_identical(dim(out), c(5L, 7L))
  expect_identical(
    out$Parameter,
    c("(Intercept)", "treatment", "gender", "treatment:gender", "Residuals")
  )
})


test_that("afex_aov, p-adjustement", {
  skip_if_not_installed("afex")
  data(laptop_urry, package = "afex")
  afx <- afex::aov_4(
    overall ~ condition * talk + (1 | pid),
    data = laptop_urry,
    anova_table = list(p_adjust_method = "bonferroni")
  )
  out1 <- model_parameters(afx, ci = 0.95)
  out2 <- model_parameters(afx, ci = 0.95, p_adjust = "bonferroni")

  expect_identical(dim(out1), c(4L, 7L))
  expect_equal(out1$Sum_Squares, c(115.01087, 6703.72241, 1944.0391, 29101.23396), tolerance = 1e-3)
  expect_named(
    out1,
    c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p", "Method")
  )
  expect_equal(out1$p, c(1, 0, 0.2157, NA), tolerance = 1e-3)
  expect_equal(out2$p, c(1, 0, 0.2157, NA), tolerance = 1e-3)

  afx <- afex::aov_4(
    overall ~ condition * talk + (1 | pid),
    data = laptop_urry
  )
  out3 <- model_parameters(afx, ci = 0.95)
  out4 <- model_parameters(afx, ci = 0.95, p_adjust = "bonferroni")
  expect_equal(out3$p, c(0.4714, 0, 0.0719, NA), tolerance = 1e-3)
  expect_equal(out4$p, c(1, 0, 0.2157, NA), tolerance = 1e-3)
})


test_that("afex_aov_ez, p-adjustement", {
  skip_if_not_installed("afex")
  data(obk.long, package = "afex")
  a2 <- afex::aov_ez(
    "id",
    "value",
    data = obk.long,
    between = c("treatment", "gender"),
    within = c("phase", "hour"),
    observed = "gender",
    anova_table = list(p_adjust_method = "fdr")
  )

  out <- model_parameters(a2)
  expect_equal(a2$anova_table$`Pr(>F)`, out$p, tolerance = 1e-4)
  expect_identical(dim(out), c(15L, 9L))
  expect_named(
    out,
    c(
      "Parameter", "Sum_Squares", "Sum_Squares_Error", "df", "df_error",
      "Mean_Square", "F", "p", "Method"
    )
  )
})
