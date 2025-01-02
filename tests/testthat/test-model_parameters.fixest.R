test_that("model_parameters.fixest", {
  skip_on_cran()
  skip_if_not_installed("fixest")
  skip_if_not_installed("carData")

  # avoid warnings
  fixest::setFixest_nthreads(1)

  data("qol_cancer")
  data(trade, package = "fixest")
  data(Greene, package = "carData")
  data(iris)

  d <- Greene
  d$dv <- as.numeric(Greene$decision == "yes")

  qol_cancer <- cbind(
    qol_cancer,
    datawizard::demean(qol_cancer, select = c("phq4", "QoL"), by = "ID")
  )

  m1 <- fixest::feols(QoL ~ time + phq4 | ID, data = qol_cancer)
  m2 <- fixest::femlm(Euros ~ log(dist_km) | Origin + Destination + Product, data = trade)
  m3 <- fixest::femlm(log1p(Euros) ~ log(dist_km) | Origin + Destination + Product, data = trade, family = "gaussian")
  m4 <- fixest::feglm(Euros ~ log(dist_km) | Origin + Destination + Product, data = trade, family = "poisson")
  m5 <- fixest::feols(Sepal.Width ~ Petal.Length | Species | Sepal.Length ~ Petal.Width, data = iris)
  m6 <- fixest::feglm(dv ~ language | judge, data = d, cluster = "judge", family = "logit")

  params <- model_parameters(m1, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(2L, 9L))
  expect_equal(params$p, as.vector(fixest::pvalue(m1)), tolerance = 1e-4)
  expect_equal(params$df_error[1], as.vector(fixest::degrees_freedom(m1, type = "t")), tolerance = 1e-4)
  expect_equal(params$Coefficient, as.vector(coef(m1)), tolerance = 1e-4)

  # currently, a bug for fixest 10.4 on R >= 4.3
  # skip_if_not(getRversion() < "4.2.0")
  expect_snapshot(
    model_parameters(m1, include_info = TRUE, verbose = FALSE)
  )

  # Poission, df = Inf
  params <- model_parameters(m2, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(1L, 9L))
  expect_equal(params$p, as.vector(fixest::pvalue(m2)), tolerance = 1e-4)
  expect_identical(params$df_error[1], Inf)
  expect_equal(params$Coefficient, as.vector(coef(m2)), tolerance = 1e-4)

  params <- model_parameters(m3, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(1L, 9L))
  expect_equal(params$p, as.vector(fixest::pvalue(m3)), tolerance = 1e-4)
  expect_equal(params$df_error[1], as.vector(fixest::degrees_freedom(m3, type = "t")), tolerance = 1e-4)
  expect_equal(params$Coefficient, as.vector(coef(m3)), tolerance = 1e-4)

  # Poission, df = Inf
  params <- model_parameters(m4, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(1L, 9L))
  expect_equal(params$p, as.vector(fixest::pvalue(m4)), tolerance = 1e-4)
  expect_identical(params$df_error[1], Inf)
  expect_equal(params$Coefficient, as.vector(coef(m4)), tolerance = 1e-4)

  params <- model_parameters(m5, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(2L, 9L))
  expect_equal(params$p, as.vector(fixest::pvalue(m5)), tolerance = 1e-4)
  expect_equal(params$df_error[1], as.vector(fixest::degrees_freedom(m5, type = "t")), tolerance = 1e-4)
  expect_equal(params$Coefficient, as.vector(coef(m5)), tolerance = 1e-4)

  # logit, df = Inf
  params <- model_parameters(m6, verbose = FALSE)
  expect_identical(c(nrow(params), ncol(params)), c(1L, 9L))
  expect_equal(params$p, as.vector(fixest::pvalue(m6)), tolerance = 1e-4)
  expect_identical(params$df_error[1], Inf)
  expect_equal(params$Coefficient, as.vector(coef(m6)), tolerance = 1e-4)
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
  expect_equal(out1$p, as.vector(fixest::pvalue(mod1)), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(out1$SE, as.vector(sqrt(diag(vcov(mod1)))), tolerance = 1e-4, ignore_attr = TRUE)
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


test_that("standard errors, Sun and Abraham", {
  skip_if_not_installed("did")
  data(mpdta, package = "did")
  m <- fixest::feols(
    lemp ~ sunab(first.treat, year, ref.p = -1:-4, att = TRUE) | countyreal + year,
    data = mpdta,
    cluster = ~countyreal
  )
  out <- model_parameters(m)
  expect_equal(out$SE, m$coeftable[, "Std. Error"], tolerance = 1e-4, ignore_attr = TRUE)

  data(base_stagg, package = "fixest")
  m <- fixest::feols(y ~ x1 + sunab(year_treated, year) | id + year, base_stagg)
  out <- model_parameters(m)
  expect_equal(out$SE, m$coeftable[, "Std. Error"], tolerance = 1e-4, ignore_attr = TRUE)
})


skip_if_not_installed("withr")
skip_if_not_installed("glmmTMB")

withr::with_options(
  list(parameters_warning_exponentiate = TRUE),
  test_that("model_parameters works for fixest-negbin", {
    data(Salamanders, package = "glmmTMB")
    mod <- fixest::fenegbin(count ~ mined + spp, data = Salamanders)
    out <- model_parameters(mod)
    expect_snapshot(print(out))
  })
)
