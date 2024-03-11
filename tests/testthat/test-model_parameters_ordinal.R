skip_on_cran()

skip_if_not_installed("ordinal")
d <- data.frame(
  Stim = c(
    "New", "New", "New", "New", "New", "New",
    "Old", "Old", "Old", "Old", "Old", "Old"
  ),
  Response = c(
    "Confidence1", "Confidence2", "Confidence3", "Confidence4",
    "Confidence5", "Confidence6", "Confidence1", "Confidence2",
    "Confidence3", "Confidence4", "Confidence5", "Confidence6"
  ),
  w = c(320, 295, 243, 206, 174, 159, 136, 188, 208, 256, 302, 333),
  stringsAsFactors = FALSE
)

m1 <- ordinal::clm(ordered(Response) ~ Stim,
  scale = ~Stim,
  link = "probit",
  data = d, weights = w
)

m2 <- ordinal::clm2(ordered(Response) ~ Stim,
  scale = ~Stim,
  link = "probit",
  data = d, weights = w
)


test_that("model_parameters.clm", {
  mp <- model_parameters(m1)
  expect_equal(
    mp$Parameter,
    c(
      "Confidence1|Confidence2", "Confidence2|Confidence3", "Confidence3|Confidence4",
      "Confidence4|Confidence5", "Confidence5|Confidence6", "StimOld",
      "StimOld"
    ),
    tolerance = 1e-4
  )
  expect_equal(
    mp$Component,
    c("intercept", "intercept", "intercept", "intercept", "intercept", "location", "scale"),
    tolerance = 1e-4
  )
  expect_equal(
    mp$Coefficient,
    c(-0.72845, -0.15862, 0.26583, 0.69614, 1.23477, 0.55237, -0.04069),
    tolerance = 1e-4
  )

  mp <- model_parameters(m1, exponentiate = TRUE)
  expect_equal(
    mp$Coefficient,
    c(0.48266, 0.85332, 1.30451, 2.006, 3.4376, 0.55237, -0.04069),
    tolerance = 1e-4
  )

  expect_snapshot(print(mp))
})

test_that("model_parameters.clm2", {
  mp <- model_parameters(m2)
  expect_equal(
    mp$Parameter,
    c(
      "Confidence1|Confidence2", "Confidence2|Confidence3", "Confidence3|Confidence4",
      "Confidence4|Confidence5", "Confidence5|Confidence6", "StimOld",
      "StimOld"
    ),
    tolerance = 1e-4
  )
  expect_equal(
    mp$Component,
    c("intercept", "intercept", "intercept", "intercept", "intercept", "location", "scale"),
    tolerance = 1e-4
  )
  expect_equal(
    mp$Coefficient,
    c(-0.72845, -0.15862, 0.26583, 0.69614, 1.23477, 0.55237, -0.04069),
    tolerance = 1e-4
  )

  mp <- model_parameters(m2, exponentiate = TRUE)
  expect_equal(
    mp$Coefficient,
    c(0.48266, 0.85332, 1.30451, 2.006, 3.4376, 0.55237, -0.04069),
    tolerance = 1e-4
  )

  expect_snapshot(print(mp))
})

test_that("model_parameters.clmm, exponentiate works w/o component column", {
  data(wine, package = "ordinal")
  mox <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)
  out1 <- model_parameters(mox, exponentiate = FALSE)
  out2 <- model_parameters(mox, exponentiate = TRUE)
  expect_equal(out1$Coefficient, c(-1.62367, 1.51337, 4.22853, 6.08877, 3.063, 1.83488, 1.13113), tolerance = 1e-4)
  expect_equal(out2$Coefficient, c(0.19717, 4.54199, 68.61606, 440.87991, 21.39156, 6.26441, 1.13113), tolerance = 1e-4)
  expect_identical(attributes(out1)$coefficient_name, "Log-Odds")
  expect_identical(attributes(out2)$coefficient_name, "Odds Ratio")
})
