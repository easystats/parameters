if (requiet("testthat") &&
  requiet("parameters") &&
  requiet("stats") &&
  requiet("plm") &&
  getRversion() > "3.5") {
  data(Crime)
  data("Produc", package = "plm")
  set.seed(123)

  Crime$year <- as.factor(Crime$year)

  m1 <- plm(lcrmrte ~ lprbarr + year | . - lprbarr + lmix, data = Crime, model = "random")

  m2 <- plm::plm(
    formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
    data = Produc,
    index = c("state", "year")
  )

  test3333 <- data.frame(
    ID = c("TOM", "TOM", "TOM", "TOM", "MARY", "MARY", "MARY", "JOHN", "JOHN"),
    Year = c(1992:1995, 1991:1993, 1993:1994),
    ret = rnorm(9)
  )
  test3333 <- pdata.frame(test3333)
  test3333["lag"] <- lag(test3333$ret)
  test3333 <- na.omit(test3333)
  test3333model <- ret ~ lag
  m3 <- plm::plm(
    test3333model,
    data = test3333,
    model = "within",
    effect = "individual",
    index = c("ID", "Year")
  )

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(-3.73774, -0.12257, -0.0596, -0.13346, -0.1837, -0.17772, -0.11678, -0.03952),
      tolerance = 1e-3
    )
    expect_equal(
      ci(m2)$CI_low,
      c(-0.08299, 0.24277, 0.70918, -0.00724),
      tolerance = 1e-3
    )
    expect_equal(ci(m3)$CI_low, -1.394381, tolerance = 1e-3)
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
      c(0, 0.52827, 0.79447, 0.00252, 0, 0, 0.0153, 0.63378),
      tolerance = 1e-3
    )
    expect_equal(
      p_value(m2)$p,
      c(0.36752, 0, 0, 0),
      tolerance = 1e-3
    )
    expect_equal(p_value(m3)$p, 0.5369632, tolerance = 1e-3)
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
