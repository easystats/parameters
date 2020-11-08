if (require("testthat") && require("parameters")) {
  test_that("model_parameters.pairwise.htest", {
    data(airquality)
    airquality$Month <- factor(airquality$Month, labels = month.abb[5:9])
    model <- pairwise.t.test(airquality$Ozone, airquality$Month)
    mp <- model_parameters(model)

    testthat::expect_equal(
      mp$Group1,
      c("Jun", "Jul", "Jul", "Aug", "Aug", "Aug", "Sep", "Sep", "Sep", "Sep")
    )
    testthat::expect_equal(
      mp$p,
      c(1, 0.00026, 0.05113, 0.00019, 0.04987, 1, 1, 1, 0.00488, 0.00388),
      tolerance = 1e-3
    )

    smokers <- c(83, 90, 129, 70)
    patients <- c(86, 93, 136, 82)
    model <- suppressWarnings(pairwise.prop.test(smokers, patients))
    mp <- model_parameters(model)

    testthat::expect_equal(
      mp$Group1,
      c("2", "3", "3", "4", "4", "4")
    )
    testthat::expect_equal(
      mp$p,
      c(1, 1, 1, 0.11856, 0.09322, 0.12377),
      tolerance = 1e-3
    )
  })
}
