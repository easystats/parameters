test_that("format_p_adjust", {
  expect_identical(format_p_adjust("holm"), "Holm (1979)")
  expect_identical(format_p_adjust("bonferroni"), "Bonferroni")
})
