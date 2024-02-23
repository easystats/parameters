skip_if_not_installed("serp")
skip_if_not_installed("insight", minimum_version = "0.19.8.4")
skip_if_not_installed("withr")

# make sure we have the correct interaction mark for tests
withr::with_options(
  list(parameters_interaction = "*"),
  test_that("model_parameters.serp", {
    data(wine, package = "serp")
    m1 <- serp::serp(
      rating ~ temp * contact,
      slope = "penalize",
      link = "logit", reverse = TRUE, tuneMethod = "user",
      lambda = 5, data = ordinal::wine
    )
    mp <- model_parameters(m1, verbose = FALSE)
    expect_snapshot(suppressMessages(print(mp)))

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
)
