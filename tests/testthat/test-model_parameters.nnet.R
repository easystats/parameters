skip_if_not_installed("nnet")
skip_if_not_installed("faraway")
skip_if_not(packageVersion("insight") > "0.19.1")

skip_on_cran()

test_that("model_parameters.multinom - long and wide", {
  data("cns", package = "faraway")
  cns2 <- reshape(cns,
    direction = "long", timevar = "Type",
    times = names(cns)[3:5], varying = 3:5, v.names = "Freq"
  )[, 3:6]
  cns2$Type <- factor(cns2$Type, levels = unique(cns2$Type))

  mnnet1 <- nnet::multinom(Type ~ Water + Work, data = cns2, weights = Freq, trace = FALSE)
  mpnnet1 <- model_parameters(mnnet1)

  expect_named(
    mpnnet1,
    c(
      "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
      "t", "df_error", "p", "Response"
    )
  )
  expect_identical(
    mpnnet1$Parameter,
    c("(Intercept)", "Water", "WorkNonManual", "(Intercept)", "Water","WorkNonManual")
  )
  expect_identical(
    mpnnet1$Response,
    c("Sp", "Sp", "Sp", "Other", "Other", "Other")
  )
  expect_equal(
    mpnnet1$Coefficient,
    c(0.3752, -0.0013, 0.11576, -1.12255, 0.00218, -0.27028),
    tolerance = 1e-4
  )

  mnnet2 <- nnet::multinom(cbind(An, Sp, Other) ~ Water + Work, data = cns, trace = FALSE)
  mpnnet2 <- model_parameters(mnnet2)

  expect_named(
    mpnnet2,
    c(
      "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
      "t", "df_error", "p", "Response"
    )
  )
  expect_identical(
    mpnnet2$Parameter,
    c("(Intercept)", "Water", "WorkNonManual", "(Intercept)", "Water", "WorkNonManual")
  )
  expect_identical(
    mpnnet2$Response,
    c("Sp", "Sp", "Sp", "Other", "Other", "Other")
  )
  expect_equal(
    mpnnet2$Coefficient,
    c(0.3752, -0.0013, 0.11576, -1.12255, 0.00218, -0.27028),
    tolerance = 1e-4
  )
})
