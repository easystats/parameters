test_that("pretty_names", {
  data(mtcars)
  attr(mtcars$hp, "label") <- "Gross horsepower"
  mod <- lm(mpg ~ hp + factor(cyl), mtcars)

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

  mtcars2 <- transform(mtcars, cyl = as.factor(cyl))
  attr(mtcars2$cyl, "label") <- "Cylinders"
  model <- lm(mpg ~ wt + cyl, data = mtcars2)
  p <- model_parameters(model, pretty_names = "labels", include_reference = TRUE)
  expect_identical(
    attr(p, "pretty_labels"),
    c(
      `(Intercept)` = "(Intercept)", wt = "wt", cyl4 = "Cylinders [4]",
      cyl6 = "Cylinders [6]", cyl8 = "Cylinders [8]"
    )
  )
})
