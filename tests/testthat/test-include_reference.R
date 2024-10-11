skip_if_not_installed("tinytable")

test_that("include_reference, on-the-fly factors", {
  data(mtcars)
  d <- as.data.frame(mtcars)
  d$gear <- as.factor(d$gear)
  d$am <- as.factor(d$am)

  m1 <- lm(mpg ~ as.factor(gear) + factor(am) + hp, data = mtcars)
  m2 <- lm(mpg ~ gear + am + hp, data = d)

  out1 <- model_parameters(m1, include_reference = TRUE)
  out2 <- model_parameters(m2, include_reference = TRUE)

  expect_snapshot(print(out1))
  expect_snapshot(print(out2))

  expect_equal(attributes(out1)$pretty_names, attributes(out2)$pretty_names, ignore_attr = TRUE)
  expect_equal(out1$Coefficient, out2$Coefficient, tolerance = 1e-4)

  out <- compare_parameters(m1, m2, include_reference = TRUE)
  expect_snapshot(print_md(out, engine = "tt"))
})

skip_if(getRversion() < "4.3.3")
skip_if_not_installed("datawizard")

test_that("include_reference, on-the-fly factors", {
  data(mtcars)
  d <- as.data.frame(mtcars)
  d$gear <- as.factor(d$gear)
  d$am <- as.factor(d$am)

  m1 <- lm(mpg ~ as.factor(gear) + factor(am) + hp, data = mtcars)
  m2 <- lm(mpg ~ gear + am + hp, data = d)

  out1 <- model_parameters(m1, include_reference = TRUE)
  out3 <- mtcars |>
    datawizard::data_modify(gear = factor(gear), am = as.factor(am)) |>
    lm(formula = mpg ~ gear + am + hp) |>
    model_parameters(include_reference = TRUE)

  expect_equal(attributes(out1)$pretty_names, attributes(out3)$pretty_names, ignore_attr = TRUE)
})

test_that("include_reference, with pretty formatted cut", {
  data(mtcars)
  mtcars$mpg_cut <- cut(mtcars$mpg, breaks = c(0, 20, 30, 100))
  m <- lm(wt ~ mpg_cut, data = mtcars)
  out <- parameters(m, include_reference = TRUE)
  expect_identical(
    attributes(out)$pretty_names,
    c(
      `(Intercept)` = "(Intercept)", `mpg_cut(0,20]` = "mpg cut [>0-20]",
      `mpg_cut(20,30]` = "mpg cut [>20-30]", `mpg_cut(30,100]` = "mpg cut [>30-100]"
    )
  )
})
