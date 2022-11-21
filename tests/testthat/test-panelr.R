.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest &&
  requiet("testthat") &&
  requiet("parameters") &&
  requiet("panelr")) {
  data("WageData")
  wages <- panel_data(WageData, id = id, wave = t)
  m1 <- wbm(lwage ~ lag(union) + wks | blk + fem | blk * lag(union), data = wages)
  m2 <- suppressWarnings(wbm(lwage ~ lag(union) + wks | blk + fem | blk * (t | id), data = wages))

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(0.00807, -0.00376, 6.14479, -0.09624, -0.00507, -0.34607, -0.53918, -0.37071),
      tolerance = 1e-3
    )
    expect_equal(
      ci(m2)$CI_low,
      c(-0.01668, -0.00139, 6.01762, -0.08795, -0.0055, -0.32126, -0.54359),
      tolerance = 1e-3
    )
  })

  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.0256, 0.00108, 0.2313, 0.03482, 0.00482, 0.05952, 0.04971, 0.12418),
      tolerance = 1e-3
    )
    expect_equal(
      standard_error(m2)$SE,
      c(0.01838, 0.00073, 0.22549, 0.03394, 0.0047, 0.05803, 0.04846),
      tolerance = 1e-3
    )
  })

  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0.02295, 0.13007, 0, 0.42167, 0.36422, 0.00013, 0, 0.30533),
      tolerance = 1e-3
    )
    expect_equal(
      p_value(m2)$p,
      c(0.29282, 0.9538, 0, 0.52805, 0.43004, 0.00038, 0),
      tolerance = 1e-3
    )
  })

  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1, effects = "fixed")$Coefficient,
      c(0.05825, -0.00164, 6.59813, -0.028, 0.00438, -0.22941, -0.44176, -0.12732),
      tolerance = 1e-3
    )
    expect_equal(
      model_parameters(m1, effects = "all")$Coefficient,
      c(
        0.05825, -0.00164, 6.59813, -0.028, 0.00438, -0.22941, -0.44176,
        -0.12732, 0.35399, 0.23264
      ),
      tolerance = 1e-3
    )
    expect_equal(
      model_parameters(m2, effects = "fixed")$Coefficient,
      c(0.01934, 4e-05, 6.45957, -0.02143, 0.00371, -0.20753, -0.44861),
      tolerance = 1e-3
    )
  })
}
