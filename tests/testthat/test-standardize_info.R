if (requiet("nlme") && requiet("lme4")) {
  data("mtcars")
  fm1 <- lme(mpg ~ cyl, mtcars, random = ~ 1 | gear)
  fm2 <- gls(mpg ~ cyl, mtcars)

  i1 <- standardize_info(fm1)
  i2 <- standardize_info(fm2)

  test_that("standardize_info", {
    expect_equal(i1$Deviation_Response_Basic, c(sd(mtcars$mpg), sd(mtcars$mpg)), tolerance = 1e-3)
    expect_equal(i2$Deviation_Response_Basic, c(sd(mtcars$mpg), sd(mtcars$mpg)), tolerance = 1e-3)
    expect_equal(i1$Deviation_Basic, c(0, sd(mtcars$cyl)), tolerance = 1e-3)
    expect_equal(i2$Deviation_Basic, c(0, sd(mtcars$cyl)), tolerance = 1e-3)
  })
}
