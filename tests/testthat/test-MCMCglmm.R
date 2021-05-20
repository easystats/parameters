if (require("testthat") &&
  require("parameters") &&
  require("MCMCglmm")) {
  data(PlodiaPO)
  set.seed(123)
  m1 <- MCMCglmm(
    PO ~ plate,
    random = ~FSfamily,
    data = PlodiaPO,
    verbose = FALSE,
    nitt = 1300,
    burnin = 300,
    thin = 1
  )

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(0.97495, 0.03407),
      tolerance = 0.01
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.02309, 0.00509),
      tolerance = 0.01
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0),
      tolerance = 0.01
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1, centrality = "mean")$Mean,
      c(1.0132, 0.04232),
      tolerance = 0.01
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1, centrality = "median")$Median,
      c(1.01382, 0.04207),
      tolerance = 0.01
    )
  })
}
