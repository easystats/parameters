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
      "Group",
      "Level",
      "Parameter",
      "Median",
      "CI",
      "CI_low",
      "CI_high",
      "pd",
      "Component",
      "Effects"
    )
  )

  out <- parameters::model_parameters(m, effects = "grouplevel")
  expect_identical(dim(out), c(60L, 10L))
  expect_identical(unique(out$Component), c("conditional", "delta", "k"))
  expect_named(
    out,
    c(
      "Parameter",
      "Component",
      "Median",
      "CI",
      "CI_low",
      "CI_high",
      "pd",
      "Rhat",
      "ESS_tail",
      "Group"
    )
  )

  out <- parameters::model_parameters(m, effects = "all")
  expect_identical(dim(out), c(7L, 11L))
  expect_identical(unique(out$Component), c("conditional", "delta", "k", "phi"))
  expect_named(
    out,
    c(
      "Parameter",
      "Effects",
      "Component",
      "Median",
      "CI",
      "CI_low",
      "CI_high",
      "pd",
      "Rhat",
      "ESS_tail",
      "Group"
    )
  )
})


test_that("mp, studies in meta analysis correctly printed", {
  m <- suppressWarnings(insight::download_model("brms_meta_1"))
  skip_if(is.null(m))
  out <- model_parameters(m)
  expect_identical(
    attributes(out)$cleaned_parameters,
    c(
      b_Intercept = "Overall",
      `r_Author[Call.et.al.,Intercept]` = "Call et al",
      `r_Author[Cavanagh.et.al.,Intercept]` = "Cavanagh et al",
      `r_Author[DanitzOrsillo,Intercept]` = "DanitzOrsillo",
      `r_Author[de.Vibe.et.al.,Intercept]` = "de Vibe et al",
      `r_Author[Frazier.et.al.,Intercept]` = "Frazier et al",
      `r_Author[Frogeli.et.al.,Intercept]` = "Frogeli et al",
      `r_Author[Gallego.et.al.,Intercept]` = "Gallego et al",
      `r_Author[Hazlett-Stevens.&.Oren,Intercept]` = "Hazlett-Stevens & Oren",
      `r_Author[Hintz.et.al.,Intercept]` = "Hintz et al",
      `r_Author[Kang.et.al.,Intercept]` = "Kang et al",
      `r_Author[Kuhlmann.et.al.,Intercept]` = "Kuhlmann et al",
      `r_Author[Lever.Taylor.et.al.,Intercept]` = "Lever Taylor et al",
      `r_Author[Phang.et.al.,Intercept]` = "Phang et al",
      `r_Author[Rasanen.et.al.,Intercept]` = "Rasanen et al",
      `r_Author[Ratanasiripong,Intercept]` = "Ratanasiripong",
      `r_Author[Shapiro.et.al.,Intercept]` = "Shapiro et al",
      `r_Author[SongLindquist,Intercept]` = "SongLindquist",
      `r_Author[Warnecke.et.al.,Intercept]` = "Warnecke et al",
      sd_Author__Intercept = "tau"
    )
  )
})
