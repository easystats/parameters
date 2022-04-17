
# easystats convention ------------------------

mod <- parameters(stats::lm(wt ~ am * cyl, data = mtcars))

test_that("sort_parameters returns original object when no sorting - easystats", {
  expect_equal(sort_parameters(mod), mod)
})

test_that("sort_parameters returns sorted object when necessary - easystats", {
  expect_equal(
    sort_parameters(mod, sort = "ascending")$Coefficient,
    sort(mod$Coefficient)
  )

  expect_equal(
    sort_parameters(mod, sort = "descending")$Coefficient,
    sort(mod$Coefficient, decreasing = TRUE)
  )

  expect_s3_class(sort_parameters(mod, sort = "ascending"), "parameters_model")
})


# broom convention ------------------------

df <- structure(list(
  term = c("(Intercept)", "am", "cyl", "am:cyl"),
  estimate = c(
    1.65820588235294, -0.956184605757196, 0.303811274509804,
    0.0328057467667917
  ), std.error = c(
    0.587149249513266, 0.792732452856412,
    0.0826018347687406, 0.130209483362154
  ), statistic = c(
    2.82416418606949,
    -1.20618829506957, 3.67802089820863, 0.251945909926916
  ),
  p.value = c(
    0.00863653784417726, 0.237838251537444, 0.000989221758576308,
    0.802923027949227
  )
), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -4L))

test_that("sort_parameters returns original object when no sorting - broom", {
  expect_equal(sort_parameters(df), df)
})

test_that("sort_parameters returns sorted object when necessary - broom", {
  expect_equal(
    sort_parameters(df, sort = "ascending", style = "broom")$estimate,
    sort(df$estimate)
  )

  expect_equal(
    sort_parameters(df, sort = "descending", style = "broom")$estimate,
    sort(df$estimate, decreasing = TRUE)
  )

  expect_s3_class(sort_parameters(df, sort = "ascending", style = "broom"), "tbl_df")
})
