if (require("testthat") && require("parameters")) {
  set.seed(123)
  expect_snapshot(parameters(lm(formula = wt ~ am * cyl, data = mtcars)))
}
