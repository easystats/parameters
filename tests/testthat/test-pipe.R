skip_on_cran()
skip_if(getRversion() < "4.2.0")

test_that("print in pipe", {
  data(iris)
  out <- capture.output(lm(Sepal.Length ~ Petal.Length + Species, data = iris) |>
    model_parameters() |>
    print(include_reference = TRUE))
  expect_identical(
    out[5],
    "Species [setosa]     |        0.00 |      |                |        |       "
  )
})
