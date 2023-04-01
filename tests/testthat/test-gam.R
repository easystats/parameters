skip_if_not_installed("mgcv")
set.seed(123)
dat <- mgcv::gamSim(1, n = 400, dist = "normal", scale = 2, verbose = FALSE)
m1 <- mgcv::gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)

test_that("ci", {
  expect_equal(
    ci(m1)$CI_low,
    c(7.771085, NA, NA, NA, NA),
    tolerance = 1e-2
  )
})

test_that("se", {
  expect_equal(
    standard_error(m1)$SE,
    c(0.1020741, NA, NA, NA, NA),
    tolerance = 1e-2
  )
})

test_that("p_value", {
  expect_equal(
    p_value(m1)$p,
    c(0, 0, 0, 0, 0.00196),
    tolerance = 1e-2
  )
})

skip_on_cran()

mp <- model_parameters(m1)
test_that("model_parameters", {
  expect_snapshot(mp)
})
