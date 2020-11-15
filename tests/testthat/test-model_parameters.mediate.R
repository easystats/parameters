.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (require("testthat") && require("parameters") && require("mediation") && require("MASS")) {
  data(jobs)
  b <- lm(job_seek ~ treat + econ_hard + sex + age, data = jobs)
  c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data = jobs)
  set.seed(1234)
  m1 <- mediate(b, c, sims = 50, treat = "treat", mediator = "job_seek")

  b2 <- lm(job_seek ~ educ + sex, data = jobs)
  c2 <- lm(depress2 ~ educ + job_seek + sex, data = jobs)
  set.seed(1234)
  m2 <- mediate(b2, c2, treat = "educ", mediator = "job_seek", sims = 50,
                control.value = "gradwk", treat.value = "somcol")

  test_that("model_parameters.mediate-1", {
    params <- model_parameters(m1)
    expect_equal(params$Estimate, c(-0.01488, -0.04753, -0.06242, 0.16635), tolerance = 1e-2)
    expect_equal(params$Parameter, c("ACME", "ADE", "Total Effect", "Prop. Mediated"))
  })

  test_that("model_parameters.mediate-2", {
    params <- model_parameters(m2)
    expect_equal(params$Estimate, c(0.02484, -0.05793, -0.03309, -0.27914), tolerance = 1e-2)
    expect_equal(params$Parameter, c("ACME", "ADE", "Total Effect", "Prop. Mediated"))
  })

  if (.runThisTest) {
    jobs$job_disc <- as.factor(jobs$job_disc)
    b.ord <- MASS::polr(
      job_disc ~ treat + econ_hard + sex + age,
      data = jobs,
      method = "probit",
      Hess = TRUE
    )
    d.bin <- glm(
      work1 ~ treat + job_disc + econ_hard + sex + age,
      data = jobs,
      family = binomial(link = "probit")
    )
    set.seed(1234)
    m3 <- mediate(b.ord, d.bin, sims = 50, treat = "treat", mediator = "job_disc")

    test_that("model_parameters.mediate-3", {
      params <- model_parameters(m3)

      expect_equal(params$Estimate, c(0.00216, 0.00231, 0.0486, 0.04875, 0.05091, 0.03981, 0.04829,
                                      0.00223, 0.04868, 0.04405), tolerance = 1e-2)
      expect_equal(params$Parameter, c("ACME (control)", "ACME (treated)", "ADE (control)", "ADE (treated)",
                                       "Total Effect", "Prop. Mediated (control)", "Prop. Mediated (treated)",
                                       "ACME (average)", "ADE (average)", "Prop. Mediated (average)"))
      expect_equal(params$Component, c("control", "treated", "control", "treated", "Total Effect",
                                       "control", "treated", "average", "average", "average"))
    })
  }
}
