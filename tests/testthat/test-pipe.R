skip_on_cran()
skip_if(getRversion() < "4.2.0")

test_that("print in pipe", {
  data(iris)
  out <- capture.output({
    lm(Sepal.Length ~ Petal.Length + Species, data = iris) |>
      model_parameters() |>
      print(include_reference = TRUE)
  })
  expect_identical(
    out[5],
    "Species [setosa]     |        0.00 |      |                |        |       "
  )
})


skip_if_not_installed("withr")
withr::with_options(
  list(easystats_table_width = Inf),
  test_that("print in pipe, on-the-fly factor", {
    data(mtcars)
    out <- capture.output({
      mtcars |>
        lm(mpg ~ cut(wt, c(0, 2.5, 3, 5)), data = _) |>
        model_parameters(include_reference = TRUE)
    })
    expect_identical(
      out[4],
      "cut(wt, c(0, 2.5, 3, 5)) [>0-2.5] |        0.00 |      |                 |       |       "
    )
  })
)
