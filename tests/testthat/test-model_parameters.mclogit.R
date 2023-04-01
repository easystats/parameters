skip_if_not_installed("mclogit")
skip_if_not_installed("withr")
skip_if_not(packageVersion("insight") > "0.19.1")

skip_on_cran()

withr::with_options(
  list(parameters_exponentiate = FALSE),
  {
    data(Transport, package = "mclogit")
    m1 <- mclogit::mclogit(
      cbind(resp, suburb) ~ distance + cost,
      data = Transport
    )

    data(housing, package = "MASS")
    m2 <- mclogit::mblogit(Sat ~ Infl + Type + Cont,
      weights = Freq,
      data = housing
    )

    test_that("model_parameters.mclogit", {
      params <- model_parameters(m1)
      expect_snapshot(params)
    })
    test_that("model_parameters.mblogit", {
      params <- model_parameters(m2)
      expect_snapshot(params)
    })
  }
)
