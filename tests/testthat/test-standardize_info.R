test_that("standardize_info", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  fm1 <- nlme::lme(mpg ~ cyl, mtcars, random = ~ 1 | gear)
  fm2 <- nlme::gls(mpg ~ cyl, mtcars)

  i1 <- standardize_info(fm1)
  i2 <- standardize_info(fm2)
  expect_equal(i1$Deviation_Response_Basic, c(sd(mtcars$mpg), sd(mtcars$mpg)), tolerance = 1e-3)
  expect_equal(i2$Deviation_Response_Basic, c(sd(mtcars$mpg), sd(mtcars$mpg)), tolerance = 1e-3)
  expect_equal(i1$Deviation_Basic, c(0, sd(mtcars$cyl)), tolerance = 1e-3)
  expect_equal(i2$Deviation_Basic, c(0, sd(mtcars$cyl)), tolerance = 1e-3)
})
