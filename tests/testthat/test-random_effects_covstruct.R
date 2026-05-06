skip_on_cran()

skip_on_os("mac")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("datawizard")

test_that("random effects, glmmTMB, cov-struct AR1", {
  set.seed(123)
  n_subjects <- 100
  V <- outer(1:4, 1:4, function(i, j) 0.6^abs(i - j))
  random_effects <- as.vector(t(chol(V)) %*% matrix(rnorm(n_subjects * 4), 4, n_subjects))

  dat <- data.frame(
    id = rep(1:n_subjects, each = 4),
    id2 = rep(1:(n_subjects / 2), each = 8),
    time = rep(factor(1:4), n_subjects),
    x = random_effects + rnorm(n_subjects * 4, sd = 0.3),
    y = random_effects + rnorm(n_subjects * 4, sd = 0.5)
  )

  m_ar1 <- glmmTMB::glmmTMB(y ~ 1 + ar1(time + 0 | id), data = dat)
  out <- model_parameters(m_ar1, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(out$Parameter, c("SD (time1)", "Cor (AR1 rho)", "SD (Observations)"))
  # Validated against output from VarCorr()
  expect_equal(out$Coefficient, c(0.7932, 0.6443, 0.6433), tolerance = 1e-3)

  m_ar1 <- glmmTMB::glmmTMB(y ~ 1 + ar1(time + 0 | id) + (1 | id2), data = dat)
  out <- model_parameters(m_ar1, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c("SD (Intercept)", "SD (time1)", "Cor (AR1 rho)", "SD (Observations)")
  )
  # Validated against output from VarCorr()
  expect_equal(out$Coefficient, c(1e-04, 0.7932, 0.6443, 0.6433), tolerance = 1e-3)

  m_ar1 <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + ar1(time + 0 | id) + (1 + x | id2),
    data = dat
  ))
  out <- model_parameters(m_ar1, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (time1)",
      "SD (x)",
      "Cor (AR1 rho)",
      "Cor (AR1 rho)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.1347, 0.3627, 0.8584, -1, 0.0324, 0.4545),
    tolerance = 1e-3
  )
})

test_that("random effects, glmmTMB, cov-struct CS", {
  set.seed(123)
  dat <- data.frame(
    id = rep(1:5, each = 4),
    time = rep(factor(1:4), 5),
    id2 = rep(letters[1:4], each = 5),
    x = rnorm(20),
    y = rnorm(20)
  )
  dat <- datawizard::data_modify(dat, time_num = as.numeric(time))

  m_cs <- glmmTMB::glmmTMB(y ~ 1 + cs(time + 0 | id), data = dat)
  out <- model_parameters(m_cs, effects = "random", ci_random = 0.95)

  m_cs <- glmmTMB::glmmTMB(y ~ 1 + cs(time + 0 | id) + (1 | id2), data = dat)
  out <- model_parameters(m_cs, effects = "random", ci_random = 0.95)

  m_cs <- glmmTMB::glmmTMB(y ~ 1 + cs(time + 0 | id) + (1 + x | id2), data = dat)
  out <- model_parameters(m_cs, effects = "random", ci_random = 0.95)
})
