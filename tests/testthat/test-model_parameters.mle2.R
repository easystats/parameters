if (requiet("testthat") && requiet("parameters") && requiet("bbmle")) {
  x <- 0:10
  y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
  d <- data.frame(x, y)

  LL <- function(ymax = 15, xhalf = 6) {
    -sum(stats::dpois(y, lambda = ymax / (1 + x / xhalf), log = TRUE))
  }
  model <- suppressWarnings(mle2(LL))

  test_that("model_parameters.mle2", {
    params <- model_parameters(model)
    expect_equal(params$SE, c(4.224444, 1.034797), tolerance = 1e-3)
    expect_equal(
      colnames(params),
      c("Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high", "z", "df_error", "p")
    )
  })
}
