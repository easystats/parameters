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

  data(mtcars)
  mtcars[c("gear", "cyl")] <- lapply(mtcars[c("gear", "cyl")], as.factor)
  m <- lm(mpg ~ hp + gear + cyl, data = mtcars)

  rez <- equivalence_test(m, range = c(-Inf, 0.5))
  expect_identical(
    rez$ROPE_Equivalence,
    c("Rejected", "Accepted", "Undecided", "Rejected", "Accepted", "Undecided")
  )
  rez <- equivalence_test(m, range = c(-0.5, 0.5))
  expect_identical(
    rez$ROPE_Equivalence,
    c("Rejected", "Accepted", "Undecided", "Rejected", "Rejected", "Undecided")
  )

  rez <- equivalence_test(m, range = c(-2, 2))
  expect_identical(
    rez$ROPE_Equivalence,
    c("Rejected", "Accepted", "Undecided", "Rejected", "Rejected", "Undecided")
  )
})

test_that("equivalence_test, unequal rope-range, plots", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  data(iris)
  m <- lm(Sepal.Length ~ Species, data=iris)
  rez <- equivalence_test(m, range = c(-Inf, 0.1))
  vdiffr::expect_doppelganger(
    "Equivalence-Test 1",
    plot(rez)
  )

  rez <- equivalence_test(m, range = c(-99, 0.1))
  vdiffr::expect_doppelganger(
    "Equivalence-Test 2",
    plot(rez)
  )

  data(mtcars)
  mtcars[c("gear", "cyl")] <- lapply(mtcars[c("gear", "cyl")], as.factor)
  m <- lm(mpg ~ hp + gear + cyl, data = mtcars)

  rez <- equivalence_test(m, range = c(-Inf, 0.5))
  vdiffr::expect_doppelganger(
    "Equivalence-Test 3",
    plot(rez)
  )

  rez <- equivalence_test(m, range = c(-0.5, 0.5))
  vdiffr::expect_doppelganger(
    "Equivalence-Test 4",
    plot(rez)
  )

  rez <- equivalence_test(m, range = c(-2, 2))
  vdiffr::expect_doppelganger(
    "Equivalence-Test 5",
    plot(rez)
  )
})
