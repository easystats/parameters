if (requiet("testthat") && requiet("parameters")) {
  test_that("format_p_adjust", {
    expect_equal(format_p_adjust("holm"), "Holm (1979)")
    expect_equal(format_p_adjust("bonferroni"), "Bonferroni")
  })
}
