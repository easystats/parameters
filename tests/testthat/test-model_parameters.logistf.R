skip_on_cran()

skip_if_not_installed("logistf")
skip_if_not_installed("withr")

withr::with_options(
  list(parameters_exponentiate = FALSE),
  {
    data(sex2, package = "logistf")
    m1 <- logistf::logistf(case ~ age + oc + vic + vicl + vis + dia, data = sex2)
    m2 <- logistf::flic(m1)
    m3 <- logistf::flac(m1, data = sex2)

    test_that("model_parameters.logistf", {
      params <- model_parameters(m1)
      expect_snapshot(params, variant = "windows")
    })
    test_that("model_parameters.flic", {
      params <- model_parameters(m2)
      expect_snapshot(params, variant = "windows")
    })
    test_that("model_parameters.flac", {
      params <- model_parameters(m3)
      expect_snapshot(params, variant = "windows")
    })
  }
)
