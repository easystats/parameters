skip_on_cran()
skip_if(getRversion() < "4.2.0")

test_that("labelled works", {
  data(efc, package = "datawizard")
  model <- lm(neg_c_7 ~ e42dep + c172code, data = efc)

  # default printing
  expect_identical(
    capture.output(print(model_parameters(model), table_width = Inf)),
    c(
      "Parameter   | Coefficient |   SE |         95% CI | t(80) |     p",
      "-----------------------------------------------------------------",
      "(Intercept) |        8.72 | 3.56 | [ 1.63, 15.80] |  2.45 | 0.017",
      "e42dep [2]  |       -1.00 | 3.72 | [-8.41,  6.41] | -0.27 | 0.789",
      "e42dep [3]  |        2.68 | 3.16 | [-3.60,  8.96] |  0.85 | 0.398",
      "e42dep [4]  |        3.88 | 3.10 | [-2.29, 10.04] |  1.25 | 0.214",
      "c172code    |        1.14 | 0.93 | [-0.70,  2.99] |  1.23 | 0.221"
    )
  )

  # `pretty_names` in call to `model_parameters()` skips cleaning
  expect_identical(
    capture.output(print(
      model_parameters(model, pretty_names = FALSE),
      table_width = Inf
    )),
    c(
      "Parameter   | Coefficient |   SE |         95% CI | t(80) |     p",
      "-----------------------------------------------------------------",
      "(Intercept) |        8.72 | 3.56 | [ 1.63, 15.80] |  2.45 | 0.017",
      "e42dep2     |       -1.00 | 3.72 | [-8.41,  6.41] | -0.27 | 0.789",
      "e42dep3     |        2.68 | 3.16 | [-3.60,  8.96] |  0.85 | 0.398",
      "e42dep4     |        3.88 | 3.10 | [-2.29, 10.04] |  1.25 | 0.214",
      "c172code    |        1.14 | 0.93 | [-0.70,  2.99] |  1.23 | 0.221"
    )
  )

  # else, by default, we have pretty names and value labels. to print
  # value labels, need to tell this in print method
  expect_identical(
    capture.output(print(
      model_parameters(model),
      pretty_names = "labels",
      table_width = Inf
    )),
    c(
      "Parameter                                 | Coefficient |   SE |         95% CI | t(80) |     p",
      "-----------------------------------------------------------------------------------------------",
      "(Intercept)                               |        8.72 | 3.56 | [ 1.63, 15.80] |  2.45 | 0.017",
      "elder's dependency [slightly dependent]   |       -1.00 | 3.72 | [-8.41,  6.41] | -0.27 | 0.789",
      "elder's dependency [moderately dependent] |        2.68 | 3.16 | [-3.60,  8.96] |  0.85 | 0.398",
      "elder's dependency [severely dependent]   |        3.88 | 3.10 | [-2.29, 10.04] |  1.25 | 0.214",
      "carer's level of education                |        1.14 | 0.93 | [-0.70,  2.99] |  1.23 | 0.221"
    )
  )
})
