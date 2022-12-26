test_that("format_order", {
  expect_equal(format_order(2), "second")
  expect_equal(format_order(45), "forty fifth")
  expect_equal(format_order(2, textual = FALSE), "2nd")
  expect_equal(format_order(45, textual = FALSE), "45th")
})
