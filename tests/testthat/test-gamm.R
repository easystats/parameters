skip_if_not_installed("mgcv")

set.seed(123)

void <- capture.output({
  dat <- mgcv::gamSim(6, n = 200, scale = 0.2, dist = "poisson")
})
m1_gamm <- mgcv::gamm(
  y ~ s(x0) + s(x1) + s(x2),
  family = poisson,
  data = dat,
  random = list(fac = ~1),
  verbosePQL = FALSE
)

test_that("ci", {
  expect_equal(
    ci(m1_gamm)$CI_low,
    c(2.361598, NA, NA, NA),
    tolerance = 1e-3
  )
})

test_that("se", {
  expect_equal(
    standard_error(m1_gamm)$SE,
    c(0.3476989, NA, NA, NA),
    tolerance = 1e-3
  )
})

test_that("p_value", {
  expect_equal(
    p_value(m1_gamm)$p,
    c(0, 0, 0, 0),
    tolerance = 1e-3
  )
})

mp <- model_parameters(m1_gamm)
test_that("model_parameters", {
  expect_equal(
    mp$Coefficient,
    c(3.0476, NA, NA, NA),
    tolerance = 1e-3
  )
})

test_that("model_parameters", {
  expect_equal(
    mp$df,
    c(NA, 3.84696, 3.17389, 8.51855),
    tolerance = 1e-3
  )
})

test_that("model_parameters", {
  expect_equal(
    mp$df_error,
    c(183.4606, NA, NA, NA),
    tolerance = 1e-3
  )
})
