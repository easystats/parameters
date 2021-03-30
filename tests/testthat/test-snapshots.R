.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("testthat") && require("parameters")) {

  test_that("model_parameters output as expected", {
  set.seed(123)
  mod <- lm(formula = wt ~ am * cyl, data = mtcars)
  expect_snapshot(as.data.frame(parameters(mod)))
  })
}
