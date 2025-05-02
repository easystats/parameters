skip_on_cran()
skip_on_os("mac")
skip_if_not_installed("curl")
skip_if_offline()
skip_if_not_installed("httr2")
skip_if_not_installed("withr")
skip_if_not_installed("brms")
skip_if_not_installed("rstan")

withr::with_options(
  list(parameters_warning_exponentiate = TRUE),
  test_that("mp, footer exp", {
    m <- suppressWarnings(insight::download_model("brms_bernoulli_1"))
    skip_if(is.null(m))
    out <- parameters::model_parameters(m, exponentiate = FALSE)
    expect_snapshot(print(out))
    out <- parameters::model_parameters(m, exponentiate = TRUE)
    expect_snapshot(print(out))
  })
)


test_that("mp, dpars in total effects", {
  m <- suppressWarnings(insight::download_model("brms_chocomini_1"))
  skip_if(is.null(m))
  out <- parameters::model_parameters(m, effects = "total")
  expect_identical(dim(out), c(80L, 10L))
  expect_identical(unique(out$Component), c("conditional", "delta", "k", "phi"))
  expect_named(
    out,
    c(
      "Group", "Level", "Parameter", "Median", "CI", "CI_low", "CI_high",
      "pd", "Component", "Effects"
    )
  )

  out <- parameters::model_parameters(m, effects = "grouplevel")
  expect_identical(dim(out), c(60L, 10L))
  expect_identical(unique(out$Component), c("conditional", "delta", "k"))
  expect_named(
    out,
    c(
      "Group", "Level", "Parameter", "Median", "CI", "CI_low", "CI_high",
      "pd", "Component", "Effects"
    )
  )

  out <- parameters::model_parameters(m, effects = "all")
  expect_identical(dim(out), c(7L, 11L))
  expect_identical(unique(out$Component), c("conditional", "delta", "k", "phi"))
  expect_named(
    out,
    c(
      "Parameter", "Effects", "Component", "Median", "CI", "CI_low",
      "CI_high", "pd", "Rhat", "ESS", "Group"
    )
  )
})
