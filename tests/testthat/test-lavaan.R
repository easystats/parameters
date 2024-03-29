skip_if_not_installed("lavaan")

data(PoliticalDemocracy, package = "lavaan")

model <- "
    # measurement model
      ind60 =~ x1 + x2 + x3
      dem60 =~ y1 + y2 + y3 + y4
      dem65 =~ y5 + y6 + y7 + y8
    # regressions
      dem60 ~ ind60
      dem65 ~ ind60 + dem60
    # residual correlations
      y1 ~~ y5
      y2 ~~ y4 + y6
      y3 ~~ y7
      y4 ~~ y8
      y6 ~~ y8
  "

m <- lavaan::sem(model, data = PoliticalDemocracy, test = "Satorra-Bentler")

test_that("unstandardized", {
  mp <- model_parameters(m, eta_squared = "raw")
  ml <- lavaan::parameterEstimates(m, se = TRUE)
  ml <- ml[(ml$lhs != ml$rhs) & (ml$op != "~1"), ]

  expect_equal(mp$Coefficient, ml$est, tolerance = 1e-3)
  expect_equal(mp$SE, ml$se, tolerance = 1e-3)
})

test_that("standardized", {
  mp <- model_parameters(m, standardize = TRUE)
  ml <- lavaan::standardizedSolution(m, type = "std.all", se = TRUE)
  ml <- ml[(ml$lhs != ml$rhs) & (ml$op != "~1"), ]

  expect_equal(mp$Coefficient, ml$est, tolerance = 1e-3)
  expect_equal(mp$SE, ml$se, tolerance = 1e-3)
})

test_that("standardized-lv", {
  mp <- model_parameters(m, standardize = "latent")
  ml <- lavaan::standardizedSolution(m, type = "std.lv", se = TRUE)
  ml <- ml[(ml$lhs != ml$rhs) & (ml$op != "~1"), ]

  expect_equal(mp$Coefficient, ml$est, tolerance = 1e-3)
  expect_equal(mp$SE, ml$se, tolerance = 1e-3)
})

test_that("standardized-nox", {
  mp <- model_parameters(m, standardize = "no_exogenous")
  ml <- lavaan::standardizedSolution(m, type = "std.nox", se = TRUE)
  ml <- ml[(ml$lhs != ml$rhs) & (ml$op != "~1"), ]

  expect_equal(mp$Coefficient, ml$est, tolerance = 1e-3)
  expect_equal(mp$SE, ml$se, tolerance = 1e-3)
})

test_that("standardized no CI", {
  mod <- lavaan::cfa("ind60 =~ x1 + x2 + x3", data = PoliticalDemocracy)
  p <- parameters(mod, standardize = "all", ci = NULL)
  expect_s3_class(p, "parameters_sem")
})
