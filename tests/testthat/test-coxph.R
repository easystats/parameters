if (require("testthat") &&
  require("parameters") &&
  require("survival")) {
  data("lung")
  lung <- subset(lung, subset = ph.ecog %in% 0:2)
  lung$sex <- factor(lung$sex, labels = c("male", "female"))
  lung$ph.ecog <- factor(lung$ph.ecog, labels = c("good", "ok", "limited"))

  m1 <- coxph(Surv(time, status) ~ sex + age + ph.ecog, data = lung)

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(-0.87535, -0.00747, 0.01862, 0.45527),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.16823, 0.00931, 0.19961, 0.22809),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0.00118, 0.24713, 0.04005, 8e-05),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(-0.54563, 0.01078, 0.40984, 0.90232),
      tolerance = 1e-4
    )
  })
}
