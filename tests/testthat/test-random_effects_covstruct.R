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
      "Cor (Intercept~x)",
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

  m_ar1 <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + (1 + x | id2) + ar1(time + 0 | id),
    data = dat
  ))
  out <- model_parameters(m_ar1, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (x)",
      "SD (time1)",
      "Cor (Intercept~x)",
      "Cor (AR1 rho)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.1347, 0.8584, 0.3627, -1, 0.0324, 0.4545),
    tolerance = 1e-3
  )

  m_ar1 <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + (as.numeric(time) | id) + ar1(time + 0 | id),
    data = dat
  ))
  out <- model_parameters(m_ar1, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_named(
    out,
    c(
      "Parameter",
      "Coefficient",
      "SE",
      "CI",
      "CI_low",
      "CI_high",
      "Effects",
      "Group",
      "Component"
    )
  )
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (as.numeric(time))",
      "SD (time1)",
      "Cor (Intercept~as.numeric(time))",
      "Cor (AR1 rho)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.3425, 0.0429, 0.9105, 0.9374, 0.2499, 0.1166),
    tolerance = 1e-3
  )
  out <- model_parameters(m_ar1, effects = "random", ci_random = FALSE, verbose = FALSE)
  expect_named(out, c("Parameter", "Coefficient", "SE", "Effects", "Group", "Component"))
})


test_that("random effects, lme4, cov-struct AR1", {
  skip_if_not_installed("lme4")
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

  m_ar1 <- suppressWarnings(lme4::lmer(
    y ~ 1 + (as.numeric(time) | id) + ar1(time + 0 | id),
    data = dat
  ))
  out <- model_parameters(m_ar1, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_named(out, c("Parameter", "Coefficient", "SE", "Effects", "Group", "CI"))
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (as.numeric(time))",
      "SD (time1)",
      "Cor (Intercept~as.numeric(time))",
      "Cor (AR1 rho)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.3929, 0.0747, 0.9012, 0.1467, 0.242, 0.1319),
    tolerance = 1e-3
  )
  out <- model_parameters(m_ar1, effects = "random", ci_random = FALSE, verbose = FALSE)
  expect_named(out, c("Parameter", "Coefficient", "SE", "Effects", "Group", "CI"))
})

test_that("random effects, glmmTMB, cov-struct OU", {
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
  dat <- datawizard::data_modify(
    dat,
    time_num = as.numeric(time),
    pos = glmmTMB::numFactor(time_num)
  )

  m_ou <- glmmTMB::glmmTMB(y ~ 1 + ou(pos + 0 | id), data = dat)
  out <- model_parameters(m_ou, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(out$Parameter, c("SD (pos(1))", "Cor (OU decay)", "SD (Observations)"))
  # Validated against output from VarCorr()
  expect_equal(out$Coefficient, c(0.7932, 0.6443, 0.6433), tolerance = 1e-3)

  m_ou <- glmmTMB::glmmTMB(y ~ 1 + ou(pos + 0 | id) + (1 | id2), data = dat)
  out <- model_parameters(m_ou, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c("SD (Intercept)", "SD (pos(1))", "Cor (OU decay)", "SD (Observations)")
  )
  # Validated against output from VarCorr()
  expect_equal(out$Coefficient, c(1e-04, 0.7932, 0.6443, 0.6433), tolerance = 1e-3)

  m_ou <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + ou(pos + 0 | id) + (1 + x | id2),
    data = dat
  ))
  out <- model_parameters(m_ou, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (pos(1))",
      "SD (x)",
      "Cor (Intercept~x)",
      "Cor (OU decay)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.1346, 0.5689, 0.8597, -1, 0, 0.1186),
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
  out <- model_parameters(m_cs, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (time1)",
      "SD (time2)",
      "SD (time3)",
      "SD (time4)",
      "Cor (Compound Symmetry)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.8652, 1.1131, 0.7469, 0.4637, 0.3204, 0),
    tolerance = 1e-3
  )

  m_cs <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + cs(time + 0 | id) + (1 | id2),
    data = dat
  ))
  out <- model_parameters(m_cs, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (time1)",
      "SD (time2)",
      "SD (time3)",
      "SD (time4)",
      "Cor (Compound Symmetry)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0, 0.8652, 1.1131, 0.7469, 0.4637, 0.3204, 0),
    tolerance = 1e-3
  )

  m_cs <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + cs(time + 0 | id) + (1 + x | id2),
    data = dat
  ))
  out <- model_parameters(m_cs, effects = "random", ci_random = 0.95, verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (time1)",
      "SD (time2)",
      "SD (time3)",
      "SD (time4)",
      "SD (x)",
      "Cor (Intercept~x)",
      "Cor (Compound Symmetry)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.2321, 0.8412, 0.9951, 1.1163, 0.0053, 0.5507, 0.9985, 0.2508, 0),
    tolerance = 1e-3
  )
})

test_that("random effects, glmmTMB, cov-struct toeplitz", {
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
  dat <- datawizard::data_modify(dat, time_num = as.numeric(time))

  m_toep <- glmmTMB::glmmTMB(y ~ 1 + toep(time + 0 | id), data = dat)
  out <- model_parameters(m_toep, effects = "random")
  expect_identical(
    out$Parameter,
    c(
      "SD (time1)",
      "SD (time2)",
      "SD (time3)",
      "SD (time4)",
      "Cor (Toeplitz Lag 1)",
      "Cor (Toeplitz Lag 2)",
      "Cor (Toeplitz Lag 3)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(1.01122, 0.98438, 1.05246, 1.03458, 0.38933, 0.2383, 0.19787, 0.00266),
    tolerance = 1e-3
  )

  m_toep <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + toep(time + 0 | id) + (1 | id2),
    data = dat
  ))
  out <- model_parameters(m_toep, effects = "random", verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (time1)",
      "SD (time2)",
      "SD (time3)",
      "SD (time4)",
      "Cor (Toeplitz Lag 1)",
      "Cor (Toeplitz Lag 2)",
      "Cor (Toeplitz Lag 3)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0, 1.0112, 0.9844, 1.0525, 1.0346, 0.3893, 0.2383, 0.1979, 0.0041),
    tolerance = 1e-3
  )

  m_toep <- suppressWarnings(glmmTMB::glmmTMB(
    y ~ 1 + toep(time + 0 | id) + (1 + x | id2),
    data = dat
  ))
  out <- model_parameters(m_toep, effects = "random", verbose = FALSE)
  expect_identical(
    out$Parameter,
    c(
      "SD (Intercept)",
      "SD (time1)",
      "SD (time2)",
      "SD (time3)",
      "SD (time4)",
      "SD (x)",
      "Cor (Intercept~x)",
      "Cor (Toeplitz Lag 1)",
      "Cor (Toeplitz Lag 2)",
      "Cor (Toeplitz Lag 3)",
      "SD (Observations)"
    )
  )
  # Validated against output from VarCorr()
  expect_equal(
    out$Coefficient,
    c(0.131, 0.5664, 0.5578, 0.5678, 0.6301, 0.8596, -1, 0.0017, -0.0104, -0.0096, 1e-04),
    tolerance = 1e-3
  )
})
