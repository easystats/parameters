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
  expect_equal(
    parameters_type(model)$Type,
    c("intercept", "numeric", "numeric", "interaction")
  )
})


test_that("parameters_type works with logicals", {
  data(mtcars)
  tmp <- mtcars
  tmp$am <- as.logical(tmp$am)
  tmp$cyl <- as.factor(tmp$cyl)
  mod <- lm(mpg ~ am + cyl + disp, tmp)
  expect_equal(
    parameters_type(mod),
    data.frame(
      Parameter = c("(Intercept)", "amTRUE", "cyl6", "cyl8", "disp"),
      Type = c("intercept", "logical", "factor", "factor", "numeric"),
      Link = c("Mean", "Difference", "Difference", "Difference", "Association"),
      Term = c("(Intercept)", "am", "cyl6", "cyl8", "disp"),
      Variable = c(NA, "am", "cyl", "cyl", "disp"),
      Level = c(NA, "TRUE", "6", "8", NA),
      Secondary_Parameter = c(
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_,
        NA_character_
      ),
      Secondary_Type = c(NA, NA, NA, NA, NA),
      Secondary_Link = c(NA, NA, NA, NA, NA),
      Secondary_Term = c(NA, NA, NA, NA, NA),
      Secondary_Variable = c(NA, NA, NA, NA, NA),
      Secondary_Level = c(NA, NA, NA, NA, NA),
      Tertiary_Parameter = c(NA, NA, NA, NA, NA)
    ),
    ignore_attr = TRUE
  )
})
