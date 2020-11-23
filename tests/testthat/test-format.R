if (require("testthat") && require("parameters")) {
  test_that("format_order", {
    testthat::expect_equal(format_order(2), "second")
    testthat::expect_equal(format_order(45), "forty fifth")
    testthat::expect_equal(format_order(2, textual = FALSE), "2nd")
    testthat::expect_equal(format_order(45, textual = FALSE), "45th")
  })
}
