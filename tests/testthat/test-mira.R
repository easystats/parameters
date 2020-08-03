if (require("testthat") &&
  require("parameters") &&
  require("mice")) {

  data("nhanes2")
  imp <- mice(nhanes2)
  fit <- with(data = imp, exp = lm(bmi ~ age + hyp + chl))

  mp1 <- model_parameters(fit)
  mp2 <- summary(pool(fit))

  test_that("param", {
    expect_equal(mp1$Parameter, as.vector(mp2$term))
  })

  test_that("coef", {
    expect_equal(mp1$Coefficient, mp2$estimate, tolerance = 1e-3)
  })

  test_that("se", {
    expect_equal(mp1$SE, mp2$std.error, tolerance = 1e-3)
  })
}
