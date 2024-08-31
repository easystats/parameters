skip_if_not_installed("WeightIt")
skip_if_not_installed("cobalt")
skip_if_not_installed("insight", minimum_version = "0.20.4")

test_that("weightit, multinom", {
  data("lalonde", package = "cobalt")
  set.seed(1234)
  # Logistic regression ATT weights
  w.out <- WeightIt::weightit(
    treat ~ age + educ + married + re74,
    data = lalonde,
    method = "glm",
    estimand = "ATT"
  )
  lalonde$re78_3 <- factor(findInterval(lalonde$re78, c(0, 5e3, 1e4)))

  fit4 <- WeightIt::multinom_weightit(
    re78_3 ~ treat + age + educ,
    data = lalonde,
    weightit = w.out
  )
  expect_snapshot(print(model_parameters(fit4)))
})

test_that("weightit, ordinal", {
  data("lalonde", package = "cobalt")
  set.seed(1234)
  # Logistic regression ATT weights
  w.out <- WeightIt::weightit(
    treat ~ age + educ + married + re74,
    data = lalonde,
    method = "glm",
    estimand = "ATT"
  )
  lalonde$re78_3 <- factor(findInterval(lalonde$re78, c(0, 5e3, 1e4)))

  fit5 <- WeightIt::ordinal_weightit(
    ordered(re78_3) ~ treat + age + educ,
    data = lalonde,
    weightit = w.out
  )
  expect_snapshot(print(model_parameters(fit5)))
})
