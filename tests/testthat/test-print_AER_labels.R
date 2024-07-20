skip_if_not_installed("AER")
skip_if_not_installed("datawizard")

test_that("templates", {
  data(efc, package = "datawizard")
  model <- AER::tobit(neg_c_7 ~ e42dep + c172code, data = efc)
  mp <- model_parameters(model)
  expect_snapshot(print(mp, pretty_names = "labels"))
})
