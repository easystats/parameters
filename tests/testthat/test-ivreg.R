if (require("testthat") &&
  require("parameters") &&
  require("AER")) {

  data(CigarettesSW)
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m1 <- ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
    data = CigarettesSW,
    subset = year == "1995"
  )

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(7.82022, -1.79328, -0.18717),
      tolerance = 1e-4
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(1.05856, 0.2632, 0.23857),
      tolerance = 1e-4
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 1e-05, 0.24602),
      tolerance = 1e-4
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(9.89496, -1.27742, 0.2804),
      tolerance = 1e-4
    )
  })
}
