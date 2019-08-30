if (require("testthat") &&
  require("parameters") &&
  require("GLMMadaptive") &&
  require("insight")) {

  m1 <- download_model("GLMMadaptive_zi_1")
  m2 <- download_model("GLMMadaptive_1")

  test_that("ci", {
    expect_equal(
      ci(m1)$CI_low,
      c(0.78199, -0.97831, 0.67702, -0.83827, 0.6404, -1.81541),
      tolerance = 1e-4
    )
    expect_equal(
      ci(m1, component = "cond")$CI_low,
      c(0.78199, -0.97831, 0.67702),
      tolerance = 1e-4
    )
    expect_equal(
      ci(m1, component = "zi")$CI_low,
      c(-0.83827, 0.6404, -1.81541),
      tolerance = 1e-4
    )

    expect_equal(
      ci(m2)$CI_low,
      c(-1.8572, -1.59265, -1.76827, -2.41754),
      tolerance = 1e-4
    )
    expect_equal(
      ci(m2, component = "cond")$CI_low,
      c(-1.8572, -1.59265, -1.76827, -2.41754),
      tolerance = 1e-4
    )

    expect_null(ci(m2, component = "zi"))
  })



  test_that("se", {
    expect_equal(
      standard_error(m1)$SE,
      c(0.085, 0.08957, 0.09149, 0.43929, 0.23724, 0.46654),
      tolerance = 1e-4
    )
    expect_equal(
      standard_error(m1, component = "cond")$SE,
      c(0.085, 0.08957, 0.09149),
      tolerance = 1e-4
    )
    expect_equal(
      standard_error(m1, component = "zi")$SE,
      c(0.43929, 0.23724, 0.46654),
      tolerance = 1e-4
    )

    expect_equal(
      standard_error(m2)$SE,
      c(0.23354, 0.30678, 0.32678, 0.42761),
      tolerance = 1e-4
    )
    expect_equal(
      standard_error(m2, component = "cond")$SE,
      c(0.23354, 0.30678, 0.32678, 0.42761),
      tolerance = 1e-4
    )

    expect_null(standard_error(m2, component = "zi"))
  })


  test_that("p_value", {
    expect_equal(
      p_value(m1)$p,
      c(0, 0, 0, 0.95874, 0, 0.05345),
      tolerance = 1e-4
    )
    expect_equal(
      p_value(m1, component = "cond")$p,
      c(0, 0, 0),
      tolerance = 1e-4
    )
    expect_equal(
      p_value(m1, component = "zi")$p,
      c(0.95874, 0, 0.05345),
      tolerance = 1e-4
    )

    expect_equal(
      p_value(m2)$p,
      c(0, 0.00123, 0.00056, 0.00022),
      tolerance = 1e-4
    )
    expect_equal(
      p_value(m2, component = "cond")$p,
      c(0, 0.00123, 0.00056, 0.00022),
      tolerance = 1e-4
    )

    expect_null(p_value(m2, component = "zi"))
  })


  test_that("model_parameters", {
    expect_equal(
      model_parameters(m1)$Coefficient,
      c(0.9486, -0.80275, 0.85633, 0.02273, 1.10539, -0.90101),
      tolerance = 1e-4
    )
    expect_equal(
      model_parameters(m2)$Coefficient,
      c(-1.39946, -0.99138, -1.1278, -1.57945),
      tolerance = 1e-4
    )
  })
}
