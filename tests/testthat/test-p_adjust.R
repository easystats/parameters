.runThisTest <- Sys.getenv("RunAllparametersTests") == "yes"

if (.runThisTest && require("testthat") && require("parameters")) {
  data(mtcars)
  model <- lm(mpg ~ wt * cyl + am + log(hp), data = mtcars)

  test_that("model_parameters, p-adjust", {
    mp <- model_parameters(model)
    expect_equal(mp$p, c(0, 0.00304, 0.02765, 0.65851, 0.01068, 0.02312), tolerance = 1e-3)
    mp <- model_parameters(model, p_adjust = "BH")
    expect_equal(mp$p, c(0, 0.00912, 0.03318, 0.65851, 0.02137, 0.03318), tolerance = 1e-3)
    mp <- model_parameters(model, p_adjust = "bonferroni")
    expect_equal(mp$p, c(0, 0.01824, 0.16588, 1, 0.06411, 0.13869), tolerance = 1e-3)
  })
}
