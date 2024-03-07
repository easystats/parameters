test_that("print in pipe", {
  data(iris)
  model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
  out <- model_parameters(model, include_reference = TRUE)
  expect_identical(
    out$Parameter,
    c(
      "(Intercept)", "Petal.Length", "Speciessetosa", "Speciesversicolor",
      "Speciesvirginica"
    )
  )
  expect_equal(out$Coefficient, c(3.68353, 0.90456, 0, -1.60097, -2.11767), tolerance = 1e-4)
  out <- model_parameters(model, include_reference = TRUE, pretty_names = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "(Intercept)", "Petal.Length", "Speciessetosa", "Speciesversicolor",
      "Speciesvirginica"
    )
  )
  expect_equal(out$Coefficient, c(3.68353, 0.90456, 0, -1.60097, -2.11767), tolerance = 1e-4)
})

# skip_if(getRversion() < "4.0.0")

# test_that("simple reference level", {
#   data(PlantGrowth)
#   d <<- PlantGrowth
#   m <- lm(weight ~ group, data = d)
#   mp <- model_parameters(m)
#   expect_snapshot(print(mp, include_reference = TRUE))

#   data(mtcars)
#   d <<- mtcars
#   d$cyl <- as.factor(d$cyl)
#   d$am <- as.factor(d$am)
#   m <- lm(mpg ~ hp + cyl + gear + am, data = d)
#   mp <- model_parameters(m)
#   expect_snapshot(print(mp, include_reference = TRUE))

#   data(iris)
#   d <<- iris
#   m <- lm(Sepal.Length ~ Sepal.Width * Species, data = d)
#   mp <- model_parameters(m)
#   expect_snapshot(print(mp, include_reference = TRUE))

#   data(mtcars)
#   d <<- mtcars
#   d$gear <- as.factor(d$gear)
#   m <- glm(vs ~ wt + gear, data = d, family = "binomial")
#   expect_snapshot(print(model_parameters(m, exponentiate = TRUE, drop = "(Intercept)"), include_reference = TRUE))
# })

# test_that("reference for models with multiple components", {
#   skip_on_cran()
#   skip_if_not_installed("glmmTMB")
#   data("fish")

#   m1 <- glmmTMB::glmmTMB(
#     count ~ child + camper + zg + (1 | ID),
#     ziformula = ~ child + camper + (1 | persons),
#     data = fish,
#     family = glmmTMB::truncated_poisson()
#   )

#   print(model_parameters(m1), include_reference = TRUE)
# })
