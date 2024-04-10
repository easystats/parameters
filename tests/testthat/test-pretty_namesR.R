test_that("pretty_names", {
  data(mtcars)
  attr(mtcars2$hp, "label") <- "Gross horsepower"
  mod <- lm(mpg ~ hp + factor(cyl), mtcars2)

  p <- parameters::parameters(mod, pretty_names = "labels", include_reference = TRUE)
  expect_identical(
    attr(p, "pretty_labels"),
    c(
      `(Intercept)` = "(Intercept)", hp = "Gross horsepower", `factor(cyl)4` = "cyl [4]",
      `factor(cyl)6` = "cyl [6]", `factor(cyl)8` = "cyl [8]"
    )
  )

  p <- parameters::parameters(mod, pretty_names = "labels")
  expect_identical(
    attr(p, "pretty_labels"),
    c(
      `(Intercept)` = "(Intercept)", hp = "Gross horsepower", `factor(cyl)6` = "cyl [6]",
      `factor(cyl)8` = "cyl [8]"
    )
  )
})
