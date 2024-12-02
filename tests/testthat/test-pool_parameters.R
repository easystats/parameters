test_that("pooled parameters", {
  skip_if_not_installed("mice")
  data("nhanes2", package = "mice")
  set.seed(123)
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  models <- lapply(1:5, function(i) {
    lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  })
  pp <- pool_parameters(models)
  expect_equal(pp$df_error, c(9.2225, 8.1903, 3.6727, 10.264, 6.4385), tolerance = 1e-3)
  expect_snapshot(print(pp))
})

test_that("pooled parameters", {
  skip_if_not_installed("mice")
  skip_if_not_installed("datawizard")
  data("nhanes2", package = "mice")
  nhanes2$hyp <- datawizard::slide(as.numeric(nhanes2$hyp))
  set.seed(123)
  imp <- mice::mice(nhanes2, printFlag = FALSE)
  models <- lapply(1:5, function(i) {
    glm(hyp ~ age + chl, family = binomial, data = mice::complete(imp, action = i))
  })
  pp1 <- pool_parameters(models)
  expect_equal(pp1$df_error, c(Inf, Inf, Inf, Inf), tolerance = 1e-3)
  pp2 <- pool_parameters(models, ci_method = "residual")
  m_mice <- with(data = imp, exp = glm(hyp ~ age + chl, family = binomial))
  pp3 <- summary(mice::pool(m_mice))
  expect_equal(pp2$df_error, pp3$df, tolerance = 1e-3)
})

skip_on_cran()

test_that("pooled parameters, glmmTMB, components", {
  skip_if_not_installed("mice")
  skip_if_not_installed("glmmTMB")
  sim1 <- function(nfac = 4, nt = 10, facsd = 0.1, tsd = 0.15, mu = 0, residsd = 1) {
    dat <- expand.grid(fac = factor(letters[1:nfac]), t = 1:nt)
    n <- nrow(dat)
    dat$REfac <- rnorm(nfac, sd = facsd)[dat$fac]
    dat$REt <- rnorm(nt, sd = tsd)[dat$t]
    dat$x <- rnorm(n, mean = mu, sd = residsd) + dat$REfac + dat$REt
    dat
  }

  set.seed(101)
  d1 <- sim1(mu = 100, residsd = 10)
  d2 <- sim1(mu = 200, residsd = 5)
  d1$sd <- "ten"
  d2$sd <- "five"
  dat <- rbind(d1, d2)

  set.seed(101)
  dat$REfac[sample.int(nrow(dat), 10)] <- NA
  dat$x[sample.int(nrow(dat), 10)] <- NA
  dat$sd[sample.int(nrow(dat), 10)] <- NA

  impdat <- suppressWarnings(mice::mice(dat, printFlag = FALSE))
  models <- lapply(1:5, function(i) {
    glmmTMB::glmmTMB(
      x ~ sd + (1 | t),
      dispformula = ~sd,
      data = mice::complete(impdat, action = i)
    )
  })

  out <- pool_parameters(models, component = "conditional")
  expect_named(
    out,
    c(
      "Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Statistic",
      "df_error", "p"
    )
  )
  expect_equal(out$Coefficient, c(187.280225, -87.838969), tolerance = 1e-3)

  out <- suppressMessages(pool_parameters(models, component = "all", effects = "all"))
  expect_named(
    out,
    c(
      "Parameter", "Coefficient", "Effects", "SE", "CI_low", "CI_high",
      "Statistic", "df_error", "p", "Component"
    )
  )
  expect_equal(
    out$Coefficient,
    c(187.280225, -87.838969, 3.51576, -1.032665, 0.610992, NaN),
    tolerance = 1e-3
  )

  out <- pool_parameters(models, component = "all", effects = "fixed")
  expect_named(
    out,
    c(
      "Parameter", "Coefficient", "SE", "CI_low", "CI_high",
      "Statistic", "df_error", "p", "Component"
    )
  )
  expect_equal(
    out$Coefficient,
    c(187.280225, -87.838969, 3.51576, -1.032665),
    tolerance = 1e-3
  )
})


test_that("pooled parameters, glmmTMB, zero-inflated", {
  skip_if_not_installed("mice")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("broom.mixed")
  data(Salamanders, package = "glmmTMB")
  set.seed(123)
  Salamanders$cover[sample.int(nrow(Salamanders), 50)] <- NA
  Salamanders$mined[sample.int(nrow(Salamanders), 10)] <- NA

  impdat <- suppressWarnings(mice::mice(Salamanders, printFlag = FALSE))
  models <- lapply(1:5, function(i) {
    glmmTMB::glmmTMB(
      count ~ mined + cover + (1 | site),
      ziformula = ~mined,
      family = poisson(),
      data = mice::complete(impdat, action = i)
    )
  })

  out <- pool_parameters(models, ci_method = "residual")
  expect_named(
    out,
    c(
      "Parameter", "Coefficient", "SE", "CI_low", "CI_high", "Statistic",
      "df_error", "p", "Component"
    )
  )
  expect_equal(
    out$Coefficient,
    c(0.13409, 1.198551, -0.181912, 1.253029, -1.844026),
    tolerance = 1e-3
  )

  # validate against mice ---------------
  m_mice <- suppressWarnings(with(data = impdat, exp = glmmTMB::glmmTMB(
    count ~ mined + cover + (1 | site),
    ziformula = ~mined,
    family = poisson()
  )))
  mice_summ <- summary(mice::pool(m_mice, dfcom = Inf))
  expect_equal(out$Coefficient, mice_summ$estimate, tolerance = 1e-3)
  expect_equal(out$SE, mice_summ$std.error, tolerance = 1e-3)
  expect_equal(out$p, mice_summ$p.value, tolerance = 1e-3)

  out <- pool_parameters(models, component = "all", effects = "all")
  expect_named(
    out,
    c(
      "Parameter", "Coefficient", "Effects", "SE", "CI_low", "CI_high",
      "Statistic", "df_error", "p", "Component"
    )
  )
  expect_equal(
    out$Coefficient,
    c(0.13409, 1.198551, -0.181912, 1.253029, -1.844026, 0.158795),
    tolerance = 1e-3
  )

  out <- pool_parameters(models, component = "conditional", effects = "fixed")
  expect_named(
    out,
    c(
      "Parameter", "Coefficient", "SE", "CI_low", "CI_high",
      "Statistic", "df_error", "p"
    )
  )
  expect_equal(
    out$Coefficient,
    c(0.13409, 1.198551, -0.181912),
    tolerance = 1e-3
  )
})
