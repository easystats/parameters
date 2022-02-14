requiet("sandwich")
requiet("clubSandwich")
requiet("effectsize")

data(mtcars)
mtcars$am <- as.factor(mtcars$am)
model <- lm(mpg ~ wt * am + cyl + gear, data = mtcars)

test_that("model_parameters, robust", {
  params <- model_parameters(model, robust = TRUE, verbose = FALSE)
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0, 0.0259, 0.01478, 0.01197, 0.95238, 0.01165), tolerance = 1e-3)
})

test_that("model_parameters, robust CL", {
  params1 <- model_parameters(model, robust = TRUE, vcov_estimation = "CL", vcov_type = "HC1", verbose = FALSE)
  params2 <- model_parameters(model, robust = TRUE, vcov_estimation = "CL", vcov_args = list(type = "HC1"), verbose = FALSE)
  robust_se <- unname(sqrt(diag(sandwich::vcovCL(model))))
  expect_equal(params1$SE, robust_se, tolerance = 1e-3)
  expect_equal(params1$p, c(0, 0.00695, 0.00322, 0.00435, 0.94471, 0.00176), tolerance = 1e-3)
  expect_equal(params2$SE, robust_se, tolerance = 1e-3)
  expect_equal(params2$p, c(0, 0.00695, 0.00322, 0.00435, 0.94471, 0.00176), tolerance = 1e-3)
})

model2 <- lm(mpg ~ wt * am + cyl + gear, data = effectsize::standardize(mtcars))

test_that("model_parameters, robust", {
  params <- model_parameters(model, standardize = "refit", robust = TRUE, verbose = FALSE)
  robust_se <- unname(sqrt(diag(sandwich::vcovHC(model2))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0.28624, 0.0259, 0.43611, 0.01197, 0.95238, 0.01165), tolerance = 1e-3)
})


# cluster-robust standard errors, using clubSandwich
data(iris)
model <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
iris$cluster <- factor(rep(LETTERS[1:8], length.out = nrow(iris)))

test_that("model_parameters, robust CR", {
  params <- model_parameters(model, robust = TRUE, vcov_estimation = "CR", vcov_type = "CR1", vcov_args = list(cluster = iris$cluster), verbose = FALSE)
  robust_se <- unname(sqrt(diag(clubSandwich::vcovCR(model, type = "CR1", cluster = iris$cluster))))
  expect_equal(params$SE, robust_se, tolerance = 1e-3)
  expect_equal(params$p, c(0.01246, 0.04172, 0.18895, 0.57496, 0, 0), tolerance = 1e-3)
})

test_that("model_parameters, normal", {
  params <- model_parameters(model)
  expect_equal(params$p, c(0.13267, 0.21557, 0.36757, 0.77012, 3e-05, 0), tolerance = 1e-3)
})
