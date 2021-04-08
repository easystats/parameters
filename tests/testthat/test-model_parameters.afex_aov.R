if (require("testthat") &&
  require("parameters") &&
  suppressPackageStartupMessages(require("afex", quietly = TRUE))) {
  data(obk.long, package = "afex")
  m_between <- suppressWarnings(aov_car(value ~ treatment * gender + Error(id), data = obk.long))
  m_within <- suppressWarnings(aov_car(value ~ Error(id / (phase * hour)), data = obk.long))

  mp1 <- model_parameters(m_between)
  mp2 <- model_parameters(m_within)

  test_that("afex_aov", {
    expect_equal(c(nrow(mp1), ncol(mp1)), c(5, 7))
    expect_equal(mp1$Sum_Squares, c(450.62069, 11.98202, 5.56322, 8.68275, 15.2037), tolerance = 1e-3)
    expect_equal(c(nrow(mp2), ncol(mp2)), c(3, 9))
    expect_equal(mp2$Sum_Squares, c(167.5, 106.29167, 11.08333), tolerance = 1e-3)
    expect_equal(
      colnames(mp1),
      c("Parameter", "Sum_Squares", "df", "Mean_Square", "F", "p", "Method")
    )
    expect_equal(
      colnames(mp2),
      c("Parameter", "Sum_Squares", "Sum_Squares_Error", "df", "df_error", "Mean_Square", "F", "p", "Method")
    )
  })

  unloadNamespace("afex")
  unloadNamespace("lmerTest")
}
