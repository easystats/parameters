if (require("testthat") && require("parameters")) {
  if (packageVersion("insight") > "0.9.5") {
    test_that("parameters_type-1", {
      m0 <- lm(mpg ~ am * cyl, mtcars)
      m1 <- lm(mpg ~ am * scale(cyl), mtcars)
      m2 <- lm(mpg ~ scale(am) * cyl, mtcars)
      m3 <- lm(mpg ~ scale(am) * scale(cyl), mtcars)

      expect_equal(parameters_type(m0)[4, "Type"], "interaction")
      expect_equal(parameters_type(m1)[4, "Type"], "interaction")
      expect_equal(parameters_type(m2)[4, "Type"], "interaction")
      expect_equal(parameters_type(m3)[4, "Type"], "interaction")
    })


    test_that("parameters_type-2", {
      model <- lm(Sepal.Length ~ Petal.Width * scale(Petal.Length, TRUE, FALSE), data = iris)
      expect_equal(parameters_type(model)$Type, c("intercept", "numeric", "numeric", "interaction"))
    })
  }
}
