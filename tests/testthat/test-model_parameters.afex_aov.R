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
})


test_that("afex_aov", {
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
  expect_equal(out1$p, c(0.4714, 0, 0.0719, NA), tolerance = 1e-3)
  expect_equal(out2$p, c(1, 0, 0.2157, NA), tolerance = 1e-3)
})
