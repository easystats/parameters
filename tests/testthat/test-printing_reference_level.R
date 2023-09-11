# skip_if(getRversion() < "4.0.0")

# test_that("simple reference level", {
#   data(PlantGrowth)
#   d <<- PlantGrowth
#   m <- lm(weight ~ group, data = d)
#   mp <- model_parameters(m)
#   expect_snapshot(print(mp, add_reference = TRUE))

#   data(mtcars)
#   d <<- mtcars
#   d$cyl <- as.factor(d$cyl)
#   d$am <- as.factor(d$am)
#   m <- lm(mpg ~ hp + cyl + gear + am, data = d)
#   mp <- model_parameters(m)
#   expect_snapshot(print(mp, add_reference = TRUE))

#   data(iris)
#   d <<- iris
#   m <- lm(Sepal.Length ~ Sepal.Width * Species, data = d)
#   mp <- model_parameters(m)
#   expect_snapshot(print(mp, add_reference = TRUE))

#   data(mtcars)
#   d <<- mtcars
#   d$gear <- as.factor(d$gear)
#   m <- glm(vs ~ wt + gear, data = d, family = "binomial")
#   expect_snapshot(print(model_parameters(m, exponentiate = TRUE, drop = "(Intercept)"), add_reference = TRUE))
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

#   print(model_parameters(m1), add_reference = TRUE)
# })
