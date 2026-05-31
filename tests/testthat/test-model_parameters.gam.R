skip_if_not_installed("mgcv")

test_that("model_parameters.gam", {
  set.seed(123)
  model <- mgcv::gam(
    formula = mpg ~ s(hp) + s(wt) + factor(cyl) + am + qsec,
    family = stats::quasi(),
    data = mtcars
  )

  params <- model_parameters(model)
  expect_equal(
    params$SE,
    c(10.83359, 1.80704, 2.82608, 1.71366, 0.53172, NA, NA),
    tolerance = 1e-2
  )
  expect_equal(
    params$df_error,
    c(23.3923, 23.3923, 23.3923, 23.3923, 23.3923, NA, NA),
    tolerance = 1e-2
  )
  expect_equal(params$CI[[1]], 0.95, tolerance = 1e-2)
  expect_named(
    params,
    c(
      "Parameter",
      "Coefficient",
      "SE",
      "CI",
      "CI_low",
      "CI_high",
      "t / F",
      "df",
      "df_error",
      "p",
      "Component"
    )
  )
})

test_that("model_parameters.gam, re.test", {
  skip_if_not_installed("lme4")
  data("sleepstudy", package = "lme4")
  m_gam <- mgcv::gam(
    Reaction ~ poly(Days, 2) + s(Subject, bs = "re"),
    family = gaussian(),
    data = sleepstudy,
    method = "ML"
  )

  out <- model_parameters(m_gam, re_test = TRUE)
  expect_identical(dim(out), c(4L, 11L))
  out <- model_parameters(m_gam, re_test = FALSE)
  expect_identical(dim(out), c(3L, 9L))
})
