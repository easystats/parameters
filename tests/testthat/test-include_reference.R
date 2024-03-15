test_that("include_reference, on-the-fly factors", {
  data(mtcars)
  d <- as.data.frame(mtcars)
  d$gear <- as.factor(d$gear)
  d$am <- as.factor(d$am)

  m1 <- lm(mpg ~ as.factor(gear) + factor(am) + hp, data = mtcars)
  m2 <- lm(mpg ~ gear + am + hp, data = d)

  out1 <- model_parameters(m1)
  out2 <- model_parameters(m2)

  expect_snapshot(print(out1, include_reference = TRUE))
  expect_snapshot(print(out2, include_reference = TRUE))

  out1 <- model_parameters(m1, include_reference = TRUE)
  out2 <- model_parameters(m2, include_reference = TRUE)

  expect_equal(attributes(out1)$pretty_names, attributes(out2)$pretty_names, ignore_attr = TRUE)
  expect_equal(out1$Coefficient, out2$Coefficient, tolerance = 1e-4)

  skip_if_not_installed("tinytable")
  out <- compare_parameters(m1, m2, include_reference = TRUE)
  expect_snapshot(print_md(out, engine = "tt"))


  skip_if(getRversion() < "4.3.2")
  skip_if_not_installed("datawizard")
  out3 <- mtcars |>
    datawizard::data_modify(gear = factor(gear), am = as.factor(am)) |>
    lm(mpg ~ gear + am + hp, data = _) |>
    model_parameters(include_reference = TRUE)

  expect_equal(attributes(out1)$pretty_names, attributes(out3)$pretty_names, ignore_attr = TRUE)
})
