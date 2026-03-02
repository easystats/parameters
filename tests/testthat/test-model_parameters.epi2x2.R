skip_on_cran()
skip_if_not_installed("epiR")

test_that("model_parameters.epi2x2", {
  data(mtcars)
  tab <- xtabs(~ am + vs, data = mtcars)
  out <- model_parameters(epiR::epi.2by2(tab))
  expect_identical(out$Parameter, c("RR", "OR", "ARisk", "AFRisk", "PARisk", "PAFRisk"))
  expect_identical(
    attributes(out)$pretty_names,
    c(
      RR = "Risk Ratio", OR = "Odds Ratio", ARisk = "Attributable Risk",
      AFRisk = "Attributable Fraction in Exposed (%)", PARisk = "Attributable Risk in Population",
      PAFRisk = "Attributable Fraction in Population (%)"
    )
  )
})
