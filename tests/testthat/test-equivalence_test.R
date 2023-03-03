test_that("equivalence_test", {
  data(mtcars)
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  x <- equivalence_test(m)
  expect_identical(c(nrow(x), ncol(x)), c(5L, 10L))

  expect_type(capture.output(equivalence_test(m)), "character")
  expect_snapshot(print(x))
})
