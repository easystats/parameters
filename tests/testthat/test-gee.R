skip_if_not_installed("gee")
data(warpbreaks)
void <- capture.output({
  m1_gee <- suppressMessages(gee::gee(breaks ~ tension, id = wool, data = warpbreaks))
})

test_that("ci", {
  expect_equal(
    suppressMessages(ci(m1_gee))$CI_low,
    c(30.90044, -17.76184, -22.48406),
    tolerance = 1e-3
  )
})

test_that("se", {
  expect_equal(standard_error(m1_gee)$SE, c(2.80028, 3.96019, 3.96019), tolerance = 1e-3)
})

test_that("p_value", {
  expect_equal(p_value(m1_gee)$p, c(0, 0.01157, 2e-04), tolerance = 1e-3)
})

test_that("model_parameters", {
  mp <- suppressWarnings(model_parameters(m1_gee))
  expect_equal(mp$Coefficient, c(36.38889, -10, -14.72222), tolerance = 1e-3)
  expect_equal(mp$SE, c(2.80028, 3.96019, 3.96019), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(30.90044, -17.76184, -22.48406), tolerance = 1e-3)
  expect_equal(mp$p, c(0, 0.01157, 2e-04), tolerance = 1e-3)
})

test_that("model_parameters, robust", {
  expect_warning(
    {
      mp <- model_parameters(m1_gee, vcov = "HC3")
    },
    regex = "Models of class `gee` only return",
    fixed = TRUE
  )
  expect_equal(mp$Coefficient, c(36.38889, -10, -14.72222), tolerance = 1e-3)
  expect_equal(mp$SE, c(5.77471, 7.4639, 3.73195), tolerance = 1e-3)
  expect_equal(mp$CI_low, c(25.07067, -24.62898, -22.03671), tolerance = 1e-3)
  expect_equal(mp$p, c(0, 0.18032, 8e-05), tolerance = 1e-3)
})
