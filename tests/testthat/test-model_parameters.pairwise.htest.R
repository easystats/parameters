if (require("testthat") && require("parameters")) {
  test_that("model_parameters.pairwise.htest", {
    # t-test
    attach(airquality)
    Month <- factor(Month, labels = month.abb[5:9])
    x <- pairwise.t.test(Ozone, Month)
    df_t <- as.data.frame(model_parameters(x))

    testthat::expect_equal(
      df_t,
      structure(list(group1 = c(
        "Jun", "Jul", "Jul", "Aug", "Aug",
        "Aug", "Sep", "Sep", "Sep", "Sep"
      ), group2 = c(
        "May", "May",
        "Jun", "May", "Jun", "Jul", "May", "Jun", "Jul", "Aug"
      ), p.value = c(
        1,
        0.000263803612431323, 0.0511274135425034, 0.000194906071281851,
        0.0498733312537298, 1, 1, 1, 0.00487879814370623, 0.00387810836120148
      ), p.adjust.method = c(
        "holm", "holm", "holm", "holm", "holm",
        "holm", "holm", "holm", "holm", "holm"
      )), row.names = c(
        1L, 5L,
        6L, 9L, 10L, 11L, 13L, 14L, 15L, 16L
      ), class = "data.frame", na.action = structure(c(
        `2` = 2L,
        `3` = 3L, `4` = 4L, `7` = 7L, `8` = 8L, `12` = 12L
      ), class = "omit")),
      tolerance = 0.001
    )
  })
}
