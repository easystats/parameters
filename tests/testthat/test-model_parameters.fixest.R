.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && requiet("testthat") && requiet("parameters") && requiet("fixest") && requiet("datawizard") && getRversion() >= "3.6.0") {
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
    expect_equal(params$df_error[1], as.vector(fixest::degrees_freedom(m, type = "resid")), tolerance = 1e-3)
    expect_equal(params$Coefficient, as.vector(coef(m)), tolerance = 1e-3)
  })

  test_that("print digits model_parameters.fixest", {
    params <- model_parameters(m, summary = TRUE, verbose = FALSE)
    out <- capture.output(print(params))
    expect_equal(
      out[-9],
      c(
        "# Fixed Effects",
        "",
        "Parameter | Coefficient |   SE |         95% CI | t(561) |      p",
        "-----------------------------------------------------------------",
        "time      |        1.09 | 0.67 | [-0.23,  2.40] |   1.63 | 0.106 ",
        "phq4      |       -3.66 | 0.67 | [-4.98, -2.34] |  -5.45 | < .001",
        "", "Model: QoL ~ time + phq4 (564 Observations)",
        "Residual standard deviation: 12.365 (df = 561)",
        "r2: 0.743; ar2: 0.613; wr2: 0.180; war2: 0.175"
      )
    )
  })
}
