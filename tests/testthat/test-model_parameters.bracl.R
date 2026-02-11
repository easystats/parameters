skip_on_cran()

skip_if_not_installed("brglm2")
skip_if_not_installed("faraway")
skip_if_not(packageVersion("insight") > "0.19.1")

test_that("model_parameters.bracl", {
  data("cns", package = "faraway")
  cns2 <- reshape(
    cns,
    direction = "long",
    timevar = "Type",
    times = names(cns)[3:5],
    varying = 3:5,
    v.names = "Freq"
  )[, 3:6]
  cns2$Type <- factor(cns2$Type, levels = unique(cns2$Type))

  mbracl <- brglm2::bracl(Type ~ Water + Work, data = cns2, weights = Freq)
  mpbracl <- model_parameters(mbracl)

  expect_named(
    mpbracl,
    c(
      "Parameter",
      "Coefficient",
      "SE",
      "CI",
      "CI_low",
      "CI_high",
      "z",
      "df_error",
      "p",
      "Response"
    )
  )
  expect_identical(
    mpbracl$Parameter,
    c(
      "An:(Intercept)",
      "Sp:(Intercept)",
      "An:Water",
      "Sp:Water",
      "An:WorkNonManual",
      "Sp:WorkNonManual"
    )
  )
  expect_identical(mpbracl$Response, c("An", "Sp", "An", "Sp", "An", "Sp"))
  expect_equal(
    mpbracl$Coefficient,
    c(-0.37392, 1.49063, 0.00129, -0.00349, -0.11292, 0.36384),
    tolerance = 1e-4
  )
})
