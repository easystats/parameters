test_that("model_parameters.glht", {
  skip_if_not_installed("multcomp")
  set.seed(123)
  lmod <- lm(Fertility ~ ., data = swiss)
  model <- multcomp::glht(
    model = lmod,
    linfct = c(
      "Agriculture = 0",
      "Examination = 0",
      "Education = 0",
      "Catholic = 0",
      "Infant.Mortality = 0"
    )
  )
  params <- model_parameters(model)
  expect_equal(params$Coefficient, c(-0.1721, -0.258, -0.8709, 0.1041, 1.077), tolerance = 1e-2)
  expect_equal(params$SE, c(0.0703, 0.2539, 0.183, 0.0353, 0.3817), tolerance = 1e-2)
  expect_equal(
    params$Parameter,
    c("Agriculture == 0", "Examination == 0", "Education == 0", "Catholic == 0", "Infant.Mortality == 0")
  )
})

test_that("glht labels use variable labels for coefficient expressions", {
  skip_if_not_installed("multcomp")
  skip_if_not_installed("glmmTMB")

  data <- transform(mtcars, cylinder_power = cyl^2)
  attr(data$cyl, "label") <- "Cylinder gear"
  attr(data$gear, "label") <- "Forward gears"
  attr(data$cylinder_power, "label") <- "Cylinder square"
  fit <- glmmTMB::glmmTMB(mpg ~ cyl * gear + cylinder_power, data = data)
  hypothesis <- multcomp::glht(
    fit,
    linfct = c("cyl + gear = 0", "cyl:gear + cylinder_power = 0")
  )
  params <- model_parameters(hypothesis, pretty_names = "labels")

  expect_identical(
    params$Parameter,
    c("cyl + gear == 0", "cyl:gear + cylinder_power == 0")
  )
  expect_identical(
    attr(params, "pretty_labels"),
    stats::setNames(
      c(
        "Cylinder gear + Forward gears == 0",
        "Cylinder gear * Forward gears + Cylinder square == 0"
      ),
      params$Parameter
    )
  )
  expect_null(attr(model_parameters(hypothesis, pretty_names = FALSE), "pretty_labels"))
})

test_that("glht retains descriptive matrix contrast names", {
  skip_if_not_installed("multcomp")

  data <- mtcars
  attr(data$cyl, "label") <- "Cylinders"
  attr(data$gear, "label") <- "Gears"
  fit <- lm(mpg ~ cyl + gear, data = data)
  contrast <- matrix(
    rep(c(0, 1, 0), 3),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("cyl versus gear", "gear", "cyl - gear"),
      c("(Intercept)", "gear", "cyl")
    )
  )
  hypothesis <- multcomp::glht(fit, linfct = contrast)
  params <- model_parameters(hypothesis, pretty_names = "labels")

  expect_identical(unname(attr(params, "pretty_labels")), params$Parameter)
})
