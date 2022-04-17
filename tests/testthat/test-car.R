if (requiet("testthat") &&
  requiet("parameters") &&
  requiet("car")) {
  data(mtcars)
  mod <- lm(mpg ~ disp + hp, mtcars)
  x <- deltaMethod(mod, "disp + hp", rhs = 0)

  test_that("ci", {
    expect_equal(ci(x)$CI_low, x$`2.5 %`, tolerance = 1e-3)
  })

  test_that("se", {
    expect_equal(standard_error(x)$SE, x$SE, tolerance = 1e-3)
  })

  test_that("p", {
    expect_equal(p_value(x)$p, x$`Pr(>|z|)`, tolerance = 1e-3)
  })

  mp <- model_parameters(x)
  test_that("model_parameters", {
    expect_equal(mp$Coefficient, x$Estimate, tolerance = 1e-3)
    expect_equal(mp$Parameter, row.names(x), tolerance = 1e-3)
  })


  x <- deltaMethod(mod, "disp + hp", rhs = 0, level = .8)

  test_that("ci", {
    expect_equal(ci(x)$CI_low, x$`10 %`, tolerance = 1e-3)
  })

  mp <- model_parameters(x)
  test_that("model_parameters", {
    expect_equal(attributes(mp)$ci, 0.8, tolerance = 1e-3)
  })
}
