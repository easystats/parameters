if (require("insight") && require("testthat") && require("parameters")) {
  fit <- lm(cbind(mpg, disp, hp) ~ factor(cyl), data = mtcars)
  m <- aov(fit)
  mp <- model_parameters(m)

  test_that("model_parameters.maov", {
    expect_equal(
      mp$Sum_Squares,
      as.vector(do.call(c, lapply(summary(m), function(i) as.data.frame(i)$`Sum Sq`))),
      tolerance = 1e-3
    )
    expect_equal(
      mp[["F"]],
      as.vector(do.call(c, lapply(summary(m), function(i) as.data.frame(i)[["F value"]]))),
      tolerance = 1e-3
    )
  })
}
