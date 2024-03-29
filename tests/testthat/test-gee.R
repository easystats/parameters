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
  expect_equal(
    standard_error(m1_gee)$SE,
    c(2.80028, 3.96019, 3.96019),
    tolerance = 1e-3
  )
})

test_that("p_value", {
  expect_equal(
    p_value(m1_gee)$p,
    c(0, 0.01157, 2e-04),
    tolerance = 1e-3
  )
})

mp <- suppressWarnings(model_parameters(m1_gee))
test_that("model_parameters", {
  expect_equal(
    mp$Coefficient,
    c(36.38889, -10, -14.72222),
    tolerance = 1e-3
  )
})
