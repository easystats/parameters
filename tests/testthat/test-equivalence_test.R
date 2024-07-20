test_that("equivalence_test", {
  data(mtcars)
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  x <- equivalence_test(m)
  expect_identical(c(nrow(x), ncol(x)), c(5L, 9L))

  expect_type(capture.output(equivalence_test(m)), "character")
  expect_snapshot(print(x))
})

test_that("equivalence_test, unequal rope-range", {
  data(iris)
  m <- lm(Sepal.Length ~ Species, data=iris)
  rez <- equivalence_test(m, range = c(-Inf, 0.1))
  expect_identical(rez$ROPE_Equivalence, c("Rejected", "Rejected", "Rejected"))
  expect_identical(rez$ROPE_low, c(-Inf, -Inf, -Inf))

  rez <- equivalence_test(m, range = c(-99, 0.1))
  expect_identical(rez$ROPE_Equivalence, c("Rejected", "Rejected", "Rejected"))
  expect_identical(rez$ROPE_low, c(-99, -99, -99))
})
