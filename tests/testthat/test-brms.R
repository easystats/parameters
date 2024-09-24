skip_on_cran()
skip_on_os("mac")
skip_if_not_installed("httr2")
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("withr")
skip_if_not_installed("brms")
skip_if_not_installed("rstan")

withr::with_options(
  list(parameters_warning_exponentiate = TRUE),
  test_that("mp, footer exp", {
    m <- suppressWarnings(insight::download_model("brms_bernoulli_1"))
    out <- parameters::model_parameters(m, exponentiate = FALSE)
    expect_snapshot(print(out))
    out <- parameters::model_parameters(m, exponentiate = TRUE)
    expect_snapshot(print(out))
  })
)
