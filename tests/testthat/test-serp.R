skip_if_not_installed("serp")
skip_if_not_installed("insight", minimum_version = "0.19.8.4")

test_that("model_parameters.serp", {
  data(wine, package = "serp")
  m1 <- serp::serp(
    rating ~ temp * contact, slope = "penalize",
    link = "logit", reverse = TRUE, tuneMethod = "user",
    lambda = 5, data = ordinal::wine
  )
  mp <- model_parameters(m1)
  expect_snapshot(print(mp, verbose = FALSE))

  # validate against coef
  out <- coef(summary(m1))
  expect_equal(mp$Coefficient, out[, 1], tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(mp$SE, out[, 2], tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(mp$z, out[, 3], tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(mp$p, out[, 4], tolerance = 1e-4, ignore_attr = TRUE)

  out <- confint(m1)
  expect_equal(mp$CI_low, out[, 1], tolerance = 1e-4, ignore_attr = TRUE)

  expect_equal(degrees_of_freedom(m1), Inf, tolerance = 1e-3)
  expect_equal(degrees_of_freedom(m1, "residual"), 279.5938, tolerance = 1e-3)
})
