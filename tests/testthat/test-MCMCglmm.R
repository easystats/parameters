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
      c(0.976294, 0.034227),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.024089, 0.005111),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Median,
      c(1.013152, 0.042433),
      tolerance = 1e-4
    )
  })
}
