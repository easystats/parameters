data(mtcars)
model <- lm(mpg ~ wt + as.factor(gear) + am, data = mtcars)

test_that("p_calibrate model", {
  expect_silent(p_calibrate(model, verbose = FALSE))
  expect_warning(out <- p_calibrate(model))
  expect_equal(dim(out), c(5, 3))
  expect_equal(colnames(out), c("Parameter", "p", "p_calibrated"))
  expect_equal(out$p_calibrated, c(0, 5e-05, 0.48261, NA, NA), tolerance = 1e-4)
  expect_warning(out <- p_calibrate(model, type = "bayes"))
  expect_equal(out$p_calibrated, c(0, 5e-05, 0.93276, NA, NA), tolerance = 1e-4)
})


test_that("p_calibrate numeric", {
  p <- c(0.2, 0.1, 0.05, 0.01, 0.005, 0.001)
  # See Table 1 Sellke et al. doi: 10.1198/000313001300339950
  out <- p_calibrate(p)
  expect_equal(out, c(0.4667, 0.385, 0.2893, 0.1113, 0.0672, 0.0184), tolerance = 1e-3)
  out <- p_calibrate(p, type = "bayes")
  expect_equal(out, c(0.875, 0.6259, 0.4072, 0.1252, 0.072, 0.0188), tolerance = 1e-3)
})


test_that("p_calibrate print", {
  out <- p_calibrate(model, verbose = FALSE)
  ref <- capture.output(print(out))

  expect_equal(
    ref,
    c(
      "Parameter        |      p | p (calibrated)",
      "------------------------------------------",
      "(Intercept)      | < .001 |         < .001",
      "wt               | < .001 |         < .001",
      "as.factor(gear)4 | 0.242  |         0.483 ",
      "as.factor(gear)5 | 0.660  |               ",
      "am               | 0.925  |               ",
      "Calibrated p-values indicate the posterior probability of H0."
    )
  )
})
