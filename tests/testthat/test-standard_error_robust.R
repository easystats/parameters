requiet("sandwich")
requiet("clubSandwich")

mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)

test_that("sandwich", {
  # HC default
  se1 <- standard_error(mod, vcov_estimation = "vcovHC")
  se2 <- standard_error(mod, vcov_estimation = "HC3")
  se3 <- standard_error(mod, vcov_estimation = sandwich::vcovHC)
  se4 <- standard_error(mod, vcov_estimation = sandwich::vcovHC(mod))
  known <- sqrt(diag(vcovHC(mod)))
  expect_equal(se1$SE, known, ignore_attr = TRUE)
  expect_equal(se2$SE, known, ignore_attr = TRUE)
  expect_equal(se3$SE, known, ignore_attr = TRUE)
  expect_equal(se4$SE, known, ignore_attr = TRUE)

  # sandwich::vcovHC: type shortcut & manual
  se1 <- standard_error(mod, vcov_estimation = "HC4m")
  se2 <- standard_error(
      mod,
      vcov_estimation = "HC",
      vcov_args = list(type = "HC4m"))
  known <- sqrt(diag(vcovHC(mod, type = "HC4m")))
  expect_equal(se1$SE, known, ignore_attr = TRUE)
  expect_equal(se2$SE, known, ignore_attr = TRUE)

  # bootstrap
  set.seed(1024)
  se1 <- standard_error(mod, vcov_estimation = "BS", vcov_args = list(R = 100))
  set.seed(1024)
  known <- sqrt(diag(vcovBS(mod, R = 100)))
  expect_equal(se1$SE, known, ignore_attr = TRUE)
})

# clubSandwich
test_that("clubSandwich", {
  se1 <- standard_error(
      mod,
      vcov_estimation = "CR1",
      vcov_args = list(cluster = mtcars$cyl))
  se2 <- standard_error(
      mod,
      vcov_estimation = "CR",
      vcov_args = list(type = "CR1", cluster = mtcars$cyl))
  known <- sqrt(diag(vcovCR(mod, type = "CR1", cluster = mtcars$cyl)))
  expect_equal(se1$SE, known, ignore_attr = TRUE)
  expect_equal(se2$SE, known, ignore_attr = TRUE)
})
