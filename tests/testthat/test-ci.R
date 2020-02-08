if (require("testthat") && require("insight") && require("lme4") && require("parameters")) {
  data(mtcars)
  test_that("ci", {
    model <- lm(mpg ~ wt, data = mtcars)
    testthat::expect_equal(ci(model)[1, 3], 33.4505, tol = 0.01)
    testthat::expect_equal(ci(model, ci = c(0.7, 0.8))[1, 3], 35.30486, tol = 0.01)

    model <- glm(vs ~ wt, family = "binomial", data = mtcars)
    testthat::expect_equal(ci(model)[1, 3], 1.934013, tol = 0.01)

    model <- lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
    testthat::expect_equal(ci(model)[1, 3], -0.335063, tol = 0.01)

    set.seed(1)
    val <- ci(model, method = "boot")[1, 3]
    testthat::expect_equal(val, -0.555424, tol = 0.01)

    model <- insight::download_model("merMod_1")
    testthat::expect_equal(ci(model)[1, 3], -0.7876679, tol = 0.01)

    model <- insight::download_model("merMod_2")
    testthat::expect_equal(ci(model)[1, 3], -48.14195, tol = 0.01)
  })
}
