test_that("equivalence_test", {
  data(mtcars)
  m <- lm(mpg ~ gear + wt + cyl + hp, data = mtcars)
  x <- equivalence_test(m)
  expect_identical(c(nrow(x), ncol(x)), c(5L, 9L))

  expect_type(capture.output(equivalence_test(m)), "character")
  out <- capture.output(print(x))
  expect_identical(
    c(
      "# TOST-test for Practical Equivalence",
      "",
      "  ROPE: [-0.60 0.60]",
      "",
      "Parameter   |         90% CI |   SGPV | Equivalence |      p",
      "------------------------------------------------------------",
      "(Intercept) | [26.52, 46.86] | < .001 |    Rejected | > .999",
      "gear        | [-1.34,  2.07] | 0.354  |   Undecided | 0.578 ",
      "wt          | [-4.47, -1.57] | < .001 |    Rejected | 0.996 ",
      "cyl         | [-1.94,  0.32] | 0.407  |   Undecided | 0.644 ",
      "hp          | [-0.05,  0.01] | > .999 |    Accepted | < .001"
    )
  )
})
