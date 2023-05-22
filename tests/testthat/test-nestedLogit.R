skip_if_not_installed("nestedLogit")
skip_if_not_installed("broom")
skip_if_not_installed("car")
skip_if_not_installed("carData")

test_that("model_parameters.nestedLogit", {
  data(Womenlf, package = "carData")

  comparisons <- nestedLogit::logits(
    work = nestedLogit::dichotomy("not.work", working = c("parttime", "fulltime")),
    full = nestedLogit::dichotomy("parttime", "fulltime")
  )

  mnl1 <- nestedLogit::nestedLogit(
    partic ~ hincome + children,
    dichotomies = comparisons,
    data = Womenlf
  )

  out <- model_parameters(mnl1)
  expect_identical(
    out$Parameter,
    c(
      "(Intercept)", "hincome", "childrenpresent", "(Intercept)",
      "hincome", "childrenpresent"
    )
  )
  expect_equal(
    out$Coefficient,
    unname(c(coef(mnl1)[, 1], coef(mnl1)[, 2])),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    out$SE,
    unname(do.call(rbind, lapply(summary(mnl1), coef))[, "Std. Error"]),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    out$CI_low,
    c(0.60591, -0.08226, -2.16144, 2.11087, -0.18921, -3.80274),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  out <- model_parameters(mnl1, ci_method = "wald")
  expect_equal(
    out$CI_low,
    c(0.58367, -0.08108, -2.14847, 1.97427, -0.184, -3.71194),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  out <- model_parameters(mnl1, exponentiate = TRUE)
  expect_equal(
    out$Coefficient,
    exp(unname(c(coef(mnl1)[, 1], coef(mnl1)[, 2]))),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  out <- model_parameters(mnl1, vcov = "HC3")
  expect_equal(
    out$SE,
    c(0.41738, 0.02256, 0.29565, 0.76467, 0.0373, 0.56165),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  out <- model_parameters(mnl1, component = "work")
  expect_identical(nrow(out), 3L)
})


test_that("simulate_parameters.nestedLogit", {
  skip_if(getRversion() < "4.2.0")
  data(Womenlf, package = "carData")

  comparisons <- nestedLogit::logits(
    work = nestedLogit::dichotomy("not.work", working = c("parttime", "fulltime")),
    full = nestedLogit::dichotomy("parttime", "fulltime")
  )

  mnl1 <- nestedLogit::nestedLogit(
    partic ~ hincome + children,
    dichotomies = comparisons,
    data = Womenlf
  )

  set.seed(123)
  out <- simulate_parameters(mnl1, iterations = 100)
  expect_equal(
    out$Coefficient,
    c(1.35612, -0.04667, -1.59096, 3.45594, -0.10316, -2.69807),
    tolerance = 1e-3
  )
})
