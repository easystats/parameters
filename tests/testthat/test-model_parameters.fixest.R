.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && requiet("fixest")) {
  data("qol_cancer")
  qol_cancer <- cbind(
    qol_cancer,
    datawizard::demean(qol_cancer, select = c("phq4", "QoL"), group = "ID")
  )

  m <- feols(
    QoL ~ time + phq4 | ID,
    data = qol_cancer
  )
  params <- model_parameters(m, verbose = FALSE)

  test_that("model_parameters.fixest", {
    expect_equal(c(nrow(params), ncol(params)), c(2, 9))
    expect_equal(params$p, as.vector(fixest::pvalue(m)), tolerance = 1e-3)
    expect_equal(params$df_error[1], as.vector(fixest::degrees_freedom(m, type = "t")), tolerance = 1e-3)
    expect_equal(params$Coefficient, as.vector(coef(m)), tolerance = 1e-3)
  })

  # currently, a bug for fixest 10.4 on R >= 4.3
  if (getRversion() < "4.2.0") {
    test_that("print digits model_parameters.fixest", {
      params <- model_parameters(m, summary = TRUE, verbose = FALSE)
      out <- capture.output(print(params))

      expect_equal(
        out,
        c(
          "# Fixed Effects",
          "",
          "Parameter | Coefficient |   SE |         95% CI | t(187) |      p",
          "-----------------------------------------------------------------",
          "time      |        1.09 | 0.67 | [-0.23,  2.41] |   1.63 | 0.106 ",
          "phq4      |       -3.66 | 0.67 | [-4.98, -2.34] |  -5.45 | < .001",
          "",
          "Model: QoL ~ time + phq4 (564 Observations)",
          "Residual standard deviation: 12.365 (df = 561)",
          "r2: 0.743; ar2: 0.613; wr2: 0.180; war2: 0.175"
        )
      )
    })
  }
}


test_that("robust standard errors", {
  requiet("fixest")
  requiet("sandwich")
  mod <- feols(mpg ~ hp + am | cyl, data = mtcars)

  se1 <- sqrt(diag(vcov(mod)))
  se2 <- sqrt(diag(vcov(mod, vcov = "HC1")))
  se3 <- sqrt(diag(vcov(mod, vcov = ~gear)))
  expect_equal(standard_error(mod)$SE, se1, ignore_attr = TRUE)
  expect_equal(standard_error(mod, vcov = "HC1")$SE, se2, ignore_attr = TRUE)
  expect_equal(standard_error(mod, vcov = ~gear)$SE, se3, ignore_attr = TRUE)

  p1 <- p_value(mod)
  p2 <- p_value(mod, vcov = "HC1")
  p3 <- p_value(mod, vcov = ~gear)
  expect_true(all(p1$p != p2$p))
  expect_true(all(p2$p != p3$p))
  expect_true(all(p1$p != p3$p))

  expect_error(standard_error(mod, vcov = "HC3"))
  expect_error(parameters(mod, vcov = "HC3"))
  expect_error(parameters(mod, vcov = "hetero"), NA)
  expect_error(parameters(mod, vcov = "iid"), NA)
})
