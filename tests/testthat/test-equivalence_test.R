context("equivalence_test")

test_that("equivalence_test", {
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  x <- equivalence_test(m)
  testthat::expect_equal(c(nrow(x), ncol(x)), c(5, 6))

  m <- rstanarm::stan_glm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  equivalence_test(m)
})
