test_that("model_parameters.fixest", {
  skip_on_cran()
  skip_if_not_installed("fixest")
  data("qol_cancer")
  qol_cancer <- cbind(
    qol_cancer,
    datawizard::demean(qol_cancer, select = c("phq4", "QoL"), group = "ID")
  )

  m <- fixest::feols(
    QoL ~ time + phq4 | ID,
    data = qol_cancer
  )
  params <- model_parameters(m, verbose = FALSE)

  expect_identical(c(nrow(params), ncol(params)), c(2L, 9L))
  expect_equal(params$p, as.vector(fixest::pvalue(m)), tolerance = 1e-4)
  expect_equal(params$df_error[1], as.vector(fixest::degrees_freedom(m, type = "t")), tolerance = 1e-4)
  expect_equal(params$Coefficient, as.vector(coef(m)), tolerance = 1e-4)

  # currently, a bug for fixest 10.4 on R >= 4.3
  # skip_if_not(getRversion() < "4.2.0")
  expect_snapshot(
    model_parameters(m, summary = TRUE, verbose = FALSE)
  )
})


test_that("model_parameters.fixest", {
  skip_on_cran()
  skip_if_not_installed("fixest")
  skip_if_not_installed("carData")

  data(Greene, package = "carData")
  d <- Greene
  d$dv <- as.numeric(Greene$decision == "yes")

  mod1 <- fixest::feglm(dv ~ language | judge,
    data = d,
    cluster = "judge", family = "logit"
  )
  out1 <- model_parameters(mod1)
  expect_equal(out1$p, fixest::pvalue(mod1), tolerance = 1e-4)
})


test_that("robust standard errors", {
  skip_if_not_installed("fixest")
  mod <- fixest::feols(mpg ~ hp + am | cyl, data = mtcars)

  se1 <- sqrt(diag(vcov(mod)))
  se2 <- sqrt(diag(vcov(mod, vcov = "HC1")))
  se3 <- sqrt(diag(vcov(mod, vcov = ~gear)))
  expect_equal(standard_error(mod)$SE, se1, ignore_attr = TRUE)
  expect_equal(standard_error(mod, vcov = "HC1")$SE, se2, ignore_attr = TRUE)
  expect_equal(standard_error(mod, vcov = ~gear)$SE, se3, ignore_attr = TRUE)

  p1 <- p_value(mod)
  p2 <- p_value(mod, vcov = "HC1")
  p3 <- p_value(mod, vcov = ~gear)
  expect_true(all(p1$p != p2$p))
  expect_true(all(p2$p != p3$p))
  expect_true(all(p1$p != p3$p))

  expect_error(standard_error(mod, vcov = "HC3"))
  expect_error(parameters(mod, vcov = "HC3"))
  expect_error(parameters(mod, vcov = "hetero"), NA)
  expect_error(parameters(mod, vcov = "iid"), NA)
})
