if (

  requiet("stats") &&
    requiet("plm") &&
    getRversion() > "3.5") {
  data(Crime)
  data("Produc", package = "plm")
  set.seed(123)

  Crime$year <- as.factor(Crime$year)

  m1 <- suppressWarnings(plm::plm(lcrmrte ~ lprbarr + year | . - lprbarr + lmix, data = Crime, model = "random"))

  m2 <- suppressWarnings(plm::plm(
    formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
    data = Produc,
    index = c("state", "year")
  ))

  test3333 <- data.frame(
    ID = c("TOM", "TOM", "TOM", "TOM", "MARY", "MARY", "MARY", "JOHN", "JOHN"),
    Year = c(1992:1995, 1991:1993, 1993:1994),
    ret = rnorm(9)
  )
  test3333 <- pdata.frame(test3333)
  test3333["lag"] <- lag(test3333$ret)
  test3333 <- na.omit(test3333)
  test3333model <- ret ~ lag
  m3 <- suppressWarnings(plm::plm(
    test3333model,
    data = test3333,
    model = "within",
    effect = "individual",
    index = c("ID", "Year")
  ))

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(-3.73825, -0.12292, -0.05971, -0.13356, -0.18381, -0.17782, -0.11688, -0.03962),
      tolerance = 1e-3
    )
    expect_equal(
      ci(m2)$CI_low,
      c(-0.08308, 0.2427, 0.70909, -0.00724),
      tolerance = 1e-3
    )
    expect_equal(ci(m3)$CI_low, -2.60478, tolerance = 1e-3)
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.13223, 0.09221, 0.02684, 0.02679, 0.02704, 0.02671, 0.02663, 0.02664),
      tolerance = 1e-3
    )
    expect_equal(
      standard_error(m2)$SE,
      c(0.029, 0.02512, 0.03009, 0.00099),
      tolerance = 1e-3
    )
    expect_equal(standard_error(m3)$SE, 0.5166726, tolerance = 1e-3)
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0.5285, 0.79456, 0.00262, 0, 0, 0.01558, 0.63395),
      tolerance = 1e-3
    )
    expect_equal(
      p_value(m2)$p,
      c(0.36752, 0, 0, 0),
      tolerance = 1e-3
    )
    expect_equal(p_value(m3)$p, 0.53696, tolerance = 1e-3)
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(-3.47857, 0.05815, -0.00699, -0.08095, -0.13071, -0.12537, -0.06458, 0.01269),
      tolerance = 1e-3
    )
    expect_equal(
      model_parameters(m2)$Coefficient,
      c(-0.02615, 0.29201, 0.76816, -0.0053),
      tolerance = 1e-3
    )
    expect_equal(model_parameters(m3)$Coefficient, -0.381721, tolerance = 1e-3)
  })
}
