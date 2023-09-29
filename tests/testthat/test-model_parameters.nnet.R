skip_if_not_installed("nnet")
skip_if_not_installed("faraway")
skip_if_not(packageVersion("insight") > "0.19.1")

skip_on_cran()

data("cns", package = "faraway")
cns2 <- reshape(cns,
  direction = "long", timevar = "Type",
  times = names(cns)[3:5], varying = 3:5, v.names = "Freq"
)[, 3:6]
cns2$Type <- factor(cns2$Type, levels = unique(cns2$Type))

mnnet1 <- nnet::multinom(Type ~ Water + Work, data = cns2, weights = Freq, trace = FALSE)
mnnet2 <- nnet::multinom(cbind(An, Sp, Other) ~ Water + Work, data = cns, trace = FALSE)

ci1 <- confint(mnnet1)
ci2 <- confint(mnnet2)

test_that("model_parameters.multinom - long and wide", {
  mpnnet1 <- model_parameters(mnnet1)
  mpnnet2 <- model_parameters(mnnet2)

  expect_named(
    mpnnet1,
    c(
      "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
      "z", "df_error", "p", "Response"
    )
  )
  expect_identical(
    mpnnet1$Parameter,
    c("(Intercept)", "Water", "WorkNonManual", "(Intercept)", "Water", "WorkNonManual")
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
  expect_equal(
    mpnnet1$CI_low,
    as.vector(ci1[1:3, 1, 1:2]),
    tolerance = 1e-4
  )

  expect_named(
    mpnnet2,
    c(
      "Parameter", "Coefficient", "SE", "CI", "CI_low", "CI_high",
      "z", "df_error", "p", "Response"
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
  expect_equal(
    mpnnet2$CI_low,
    as.vector(ci2[1:3, 1, 1:2]),
    tolerance = 1e-4
  )
})


test_that("ci.multinom - long and wide", {
  cinnet1 <- ci(mnnet1)
  cinnet2 <- ci(mnnet2)

  expect_identical(
    cinnet1$Parameter,
    c("(Intercept)", "Water", "WorkNonManual", "(Intercept)", "Water", "WorkNonManual")
  )
  expect_identical(
    cinnet1$Response,
    c("Sp", "Sp", "Sp", "Other", "Other", "Other")
  )
  expect_equal(
    cinnet1$CI_low,
    as.vector(ci1[1:3, 1, 1:2]),
    tolerance = 1e-4
  )

  expect_identical(
    cinnet2$Parameter,
    c("(Intercept)", "Water", "WorkNonManual", "(Intercept)", "Water", "WorkNonManual")
  )
  expect_identical(
    cinnet2$Response,
    c("Sp", "Sp", "Sp", "Other", "Other", "Other")
  )
  expect_equal(
    cinnet2$CI_low,
    as.vector(ci1[1:3, 1, 1:2]),
    tolerance = 1e-4
  )
})
