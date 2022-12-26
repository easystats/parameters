data("iris")
dat <<- iris

# simple ------------------------------------------------------------------
test_that("standardize_parameters (simple)", {
  r <- as.numeric(cor.test(dat$Sepal.Length, dat$Petal.Length)$estimate)

  model <- lm(Sepal.Length ~ Petal.Length, data = dat)
  es <- standardize_parameters(model)
  expect_equal(es[2, 2], r, tolerance = 0.01)


  expect_error(standardize_parameters(model, robust = TRUE), NA)
})

# Robust ------------------------------------------------------------------
test_that("Robust post hoc", {
  model <- lm(mpg ~ hp, weights = gear, data = mtcars)
  expect_error(standardize_parameters(model, method = "basic", robust = TRUE), NA)
  expect_error(standardize_parameters(model, method = "basic", robust = TRUE, two_sd = TRUE), NA)

  model <- lm(mpg ~ hp, data = mtcars)
  expect_error(standardize_parameters(model, method = "basic", robust = TRUE), NA)
  expect_error(standardize_parameters(model, method = "basic", robust = TRUE, two_sd = TRUE), NA)
})


# model_parameters -------------------------------
test_that("standardize_parameters (model_parameters)", {
  skip_on_cran()
  model <<- lm(mpg ~ cyl + am, data = mtcars)
  mp <<- model_parameters(model, effects = "fixed")

  s1 <- standardize_parameters(model, method = "basic")
  s2 <- standardize_parameters(mp, method = "basic")

  expect_equal(s1$Parameter, s2$Parameter)
  expect_equal(s1$Std_Coefficient, s2$Std_Coefficient)
  expect_equal(s1$CI_low, s2$CI_low)
  expect_equal(s1$CI_high, s2$CI_high)

  mp_exp <<- model_parameters(model, exponentiate = TRUE, effects = "fixed")
  se1 <- standardize_parameters(model, method = "basic", exponentiate = TRUE)
  se2 <- standardize_parameters(mp_exp, method = "basic", exponentiate = TRUE)

  expect_equal(se1$Parameter, se2$Parameter)
  expect_equal(se1$Std_Coefficient, se2$Std_Coefficient)
  expect_equal(se1$CI_low, se2$CI_low)
  expect_equal(se1$CI_high, se2$CI_high)
})

# bootstrap_model ---------------------------------------------------------

test_that("standardize_parameters (bootstrap_model)", {
  skip_on_cran()
  skip_if_not_installed("boot")
  m <- lm(mpg ~ factor(cyl) + hp, mtcars)

  set.seed(1)
  bm_draws <- bootstrap_model(m, iterations = 599)
  set.seed(1)
  bm_tab <- bootstrap_parameters(m, iterations = 599)

  out_true <- standardize_parameters(m, method = "basic")
  out_boot1 <- standardize_parameters(bm_draws, method = "basic")
  out_boot2 <- standardize_parameters(bm_tab, method = "basic")

  expect_equal(out_boot1$Std_Coefficient, out_true$Std_Coefficient,
    tolerance = 0.05
  )
  expect_equal(out_boot1, out_boot2, ignore_attr = TRUE)
  expect_error(standardize_parameters(bm_draws, method = "refit"))
  expect_error(standardize_parameters(bm_tab, method = "refit"))
})



# lm with ci -----------------------------------
test_that("standardize_parameters (lm with ci)", {
  data("iris")
  model <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)

  expect_equal(
    standardize_parameters(model, method = "refit")$Std_Coefficient,
    c(0.044, -0.072, -0.060, 0.844),
    tolerance = 0.01
  )

  expect_equal(
    standardize_parameters(model, method = "posthoc")$Std_Coefficient,
    c(0, -0.072, -0.060, 0.844),
    tolerance = 0.01
  )

  expect_equal(
    standardize_parameters(model, method = "smart")$Std_Coefficient,
    c(0, -0.170, -0.142, 0.844),
    tolerance = 0.01
  )

  z_basic <- standardize_parameters(model, method = "basic")

  expect_equal(
    z_basic$Std_Coefficient,
    c(0, -0.034, -0.028, 0.844),
    tolerance = 0.01
  )

  ## CI
  expect_equal(
    z_basic$CI_low,
    c(0, -0.294, -0.433, 0.491),
    tolerance = 0.01
  )

  expect_equal(
    z_basic$CI_high,
    c(0, 0.225, 0.375, 1.196),
    tolerance = 0.01
  )

  z_basic.0.80 <- standardize_parameters(model, ci = 0.8, method = "basic")
  expect_equal(
    z_basic.0.80$CI_low,
    c(0, -0.203, -0.292, 0.614),
    tolerance = 0.01
  )

  expect_equal(
    z_basic.0.80$CI_high,
    c(0, 0.135, 0.234, 1.073),
    tolerance = 0.01
  )

  data("mtcars")
  m0 <- lm(mpg ~ cyl + factor(am), mtcars)
  expect_equal(
    standardize_parameters(m0, method = "refit")[[2]][-1],
    standardize_parameters(m0, method = "smart")[[2]][-1],
    tolerance = 0.01
  )
  expect_equal(
    standardize_parameters(m0, method = "refit", two_sd = TRUE)[[2]][-1],
    standardize_parameters(m0, method = "smart", two_sd = TRUE)[[2]][-1],
    tolerance = 0.01
  )
})


# aov ---------------------------------------------------------------------
test_that("standardize_parameters (aov)", {
  dat2 <- iris
  dat2$Cat1 <- rep(c("A", "B"), length.out = nrow(dat2))
  dat3 <<- dat2

  m_aov <- aov(Sepal.Length ~ Species * Cat1, data = dat3)
  m_lm <- lm(Sepal.Length ~ Species * Cat1, data = dat3)

  expect_equal(standardize_parameters(m_aov),
    standardize_parameters(m_lm),
    ignore_attr = TRUE
  )
})



# with function interactions" -------------------
test_that("standardize_parameters (with functions /  interactions)", {
  skip_on_cran()
  X <- scale(rnorm(100), TRUE, FALSE)
  Z <- scale(rnorm(100), TRUE, FALSE)
  Y <- scale(Z + X * Z + rnorm(100), TRUE, FALSE)

  m1 <- lm(Y ~ X * Z)
  m2 <- lm(Y ~ X * scale(Z))
  m3 <- lm(Y ~ scale(X) * Z)
  m4 <- lm(Y ~ scale(X) * scale(Z))

  expect_equal(
    standardize_parameters(m1, method = "basic")$Std_Coefficient,
    standardize_parameters(m2, method = "basic")$Std_Coefficient
  )
  expect_equal(
    standardize_parameters(m1, method = "basic")$Std_Coefficient,
    standardize_parameters(m3, method = "basic")$Std_Coefficient
  )
  # expect_equal(
  #   standardize_parameters(m1, method = "basic")$Std_Coefficient,
  #   standardize_parameters(m4, method = "basic")$Std_Coefficient
  # )


  # transformed resp or pred should not affect
  mtcars$cyl_exp <- exp(mtcars$cyl)
  mtcars$mpg_sqrt <- sqrt(mtcars$mpg)
  m1 <- lm(exp(cyl) ~ am + sqrt(mpg), mtcars)
  m2 <- lm(cyl_exp ~ am + mpg_sqrt, mtcars)

  expect_message(stdX <- standardize_parameters(m1, method = "refit"))
  expect_false(isTRUE(all.equal(
    stdX[[2]],
    standardize_parameters(m2, method = "refit")[[2]]
  )))
  expect_equal(
    standardize_parameters(m1, method = "basic")[[2]],
    standardize_parameters(m2, method = "basic")[[2]]
  )

  # posthoc / smart don't support data transformation
  expect_warning(standardize_parameters(m1, method = "smart"))
  expect_warning(standardize_parameters(m1, method = "posthoc"))
})


# exponentiate ------------------------------------------------------------
test_that("standardize_parameters (exponentiate)", {
  mod_b <- glm(am ~ mpg + cyl + hp,
    data = mtcars,
    family = poisson()
  )
  mod_refit <- standardize_parameters(mod_b, method = "refit", exponentiate = TRUE)

  expect_equal(
    mod_refit[[2]][-1],
    standardize_parameters(mod_b, method = "basic", exponentiate = TRUE)[[2]][-1]
  )
  expect_equal(
    mod_refit[[2]][-1],
    standardize_parameters(mod_b, method = "posthoc", exponentiate = TRUE)[[2]][-1]
  )
  expect_equal(
    mod_refit[[2]][-1],
    exp(standardize_parameters(mod_b, method = "basic")[[2]])[-1]
  )


  mod_b <- glm(am ~ mpg + cyl,
    data = mtcars,
    family = binomial()
  )
  mod_refit <- standardize_parameters(mod_b, method = "refit", exponentiate = TRUE)

  expect_equal(
    mod_refit[[2]][-1],
    standardize_parameters(mod_b, method = "basic", exponentiate = TRUE)[[2]][-1]
  )
  expect_equal(
    mod_refit[[2]][-1],
    standardize_parameters(mod_b, method = "posthoc", exponentiate = TRUE)[[2]][-1]
  )
  expect_equal(
    mod_refit[[2]][-1],
    exp(standardize_parameters(mod_b, method = "basic")[[2]])[-1]
  )


  mod_b <- glm(am ~ mpg + cyl + hp,
    data = mtcars,
    family = stats::gaussian()
  )
  mod_refit <- standardize_parameters(mod_b, method = "refit", exponentiate = TRUE)

  expect_equal(
    mod_refit[[2]][-1],
    standardize_parameters(mod_b, method = "basic", exponentiate = TRUE)[[2]][-1]
  )
  expect_equal(
    mod_refit[[2]][-1],
    standardize_parameters(mod_b, method = "posthoc", exponentiate = TRUE)[[2]][-1]
  )
  expect_equal(
    mod_refit[[2]][-1],
    exp(standardize_parameters(mod_b, method = "basic")[[2]])[-1]
  )
})


# Bayes ----------------------------------------
test_that("standardize_parameters (Bayes)", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")

  set.seed(1234)
  suppressWarnings(
    model <- rstanarm::stan_glm(Sepal.Length ~ Species + Petal.Width,
      data = iris,
      iter = 500, refresh = 0
    )
  )

  expect_equal(
    suppressWarnings(standardize_parameters(model, method = "refit")$Std_Median[1:4]),
    c(0.065, -0.094, -0.100, 0.862),
    tolerance = 0.01
  )

  expect_equal(
    suppressWarnings(standardize_parameters(model, method = "posthoc")$Std_Median[1:4]),
    c(0, -0.058, -0.053, 0.838),
    tolerance = 0.01
  )

  posts <- standardize_posteriors(model, method = "posthoc")
  expect_equal(dim(posts), c(1000, 4))
  expect_s3_class(posts, "data.frame")
})


# Pseudo - GLMM --------------------------------
test_that("standardize_parameters (Pseudo - GLMM)", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  set.seed(1)

  dat <- data.frame(
    X = rnorm(1000),
    Z = rnorm(1000),
    C = sample(letters[1:3], size = 1000, replace = TRUE),
    ID = sort(rep(letters, length.out = 1000))
  )
  dat <- transform(dat, Y = X + Z + rnorm(1000))
  dat <- cbind(dat, datawizard::demean(dat, c("X", "Z"), "ID"))


  m <- lme4::lmer(Y ~ scale(X_within) * X_between + C + (scale(X_within) | ID),
    data = dat
  )

  ## No robust methods... (yet)
  expect_warning(standardize_parameters(m, method = "pseudo", robust = TRUE, verbose = FALSE), regexp = "robust")


  ## Correctly identify within and between terms
  dev_resp <- standardize_info(m, include_pseudo = TRUE)$Deviation_Response_Pseudo
  expect_equal(insight::n_unique(dev_resp[c(2, 4, 5, 6)]), 1)
  expect_true(dev_resp[2] != dev_resp[3])


  ## Calc
  b <- lme4::fixef(m)[-1]
  mm <- model.matrix(m)[, -1]
  SD_x <- numeric(ncol(mm))

  SD_x[c(1, 3, 4, 5)] <- apply(mm[, c(1, 3, 4, 5)], 2, sd)
  SD_x[2] <- sd(tapply(mm[, 2], dat$ID, mean))

  m0 <- lme4::lmer(Y ~ 1 + (1 | ID), data = dat)
  m0v <- insight::get_variance(m0)
  SD_y <- c(sqrt(m0v$var.residual), sqrt(m0v$var.intercept))
  SD_y <- SD_y[c(1, 2, 1, 1, 1)]

  expect_equal(
    data.frame(Deviation_Response_Pseudo = c(SD_y[2], SD_y), Deviation_Pseudo = c(0, SD_x)),
    standardize_info(m, include_pseudo = TRUE)[, c("Deviation_Response_Pseudo", "Deviation_Pseudo")]
  )
  expect_equal(
    standardize_parameters(m, method = "pseudo")$Std_Coefficient[-1],
    unname(b * SD_x / SD_y)
  )


  ## scaling should not affect
  m1 <- lme4::lmer(Y ~ X_within + X_between + C + (X_within | ID),
    data = dat
  )
  m2 <- lme4::lmer(scale(Y) ~ X_within + X_between + C + (X_within | ID),
    data = dat
  )
  m3 <- lme4::lmer(Y ~ scale(X_within) + X_between + C + (scale(X_within) | ID),
    data = dat
  )
  m4 <- lme4::lmer(Y ~ X_within + scale(X_between) + C + (X_within | ID),
    data = dat
  )

  std1 <- standardize_parameters(m1, method = "pseudo")
  expect_equal(std1$Std_Coefficient,
    standardize_parameters(m2, method = "pseudo")$Std_Coefficient,
    tolerance = 0.001
  )
  expect_equal(std1$Std_Coefficient,
    standardize_parameters(m3, method = "pseudo")$Std_Coefficient,
    tolerance = 0.001
  )
  expect_equal(std1$Std_Coefficient,
    standardize_parameters(m4, method = "pseudo")$Std_Coefficient,
    tolerance = 0.001
  )



  ## Give warning for within that is also between
  mW <- lme4::lmer(Y ~ X_between + Z_within + C + (1 | ID), dat)
  mM <- lme4::lmer(Y ~ X + Z + C + (1 | ID), dat)

  expect_warning(standardize_parameters(mW, method = "pseudo"), regexp = NA)
  expect_warning(standardize_parameters(mM, method = "pseudo"), regexp = "within-group")
})


# ZI models ---------------------------------------------------------------
test_that("standardize_parameters (pscl)", {
  skip_on_cran()
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")

  m <- pscl::zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)

  mp <- model_parameters(m, effects = "fixed")
  sm1 <- standardize_parameters(m, method = "refit")
  expect_warning(sm2 <- standardize_parameters(m, method = "posthoc"))
  suppressWarnings({
    sm3 <- standardize_parameters(m, method = "basic")
    sm4 <- standardize_parameters(m, method = "smart")
  })

  # post hoc does it right (bar intercept)
  expect_equal(sm1$Std_Coefficient[-c(1, 6)],
    sm2$Std_Coefficient[-c(1, 6)],
    tolerance = 0.01
  )

  # basic / smart miss the ZI
  expect_equal(mp$Coefficient[6:8],
    sm3$Std_Coefficient[6:8],
    tolerance = 0.01
  )
  expect_equal(mp$Coefficient[7:8],
    sm4$Std_Coefficient[7:8],
    tolerance = 0.1
  )

  # get count numerics al right
  expect_equal(sm1$Std_Coefficient[4:5],
    sm3$Std_Coefficient[4:5],
    tolerance = 0.01
  )
  expect_equal(sm1$Std_Coefficient[4:5],
    sm4$Std_Coefficient[4:5],
    tolerance = 0.01
  )
})

test_that("include_response | (g)lm", {
  # lm ---
  data(iris)
  iris$Sepal.Length <- iris$Sepal.Length * 5
  m <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)

  m_z <- datawizard::standardize(m, include_response = FALSE)
  par_z0 <- standardize_parameters(m, method = "basic")
  par_z1 <- standardize_parameters(m, include_response = FALSE)
  par_z2 <- standardize_parameters(m, method = "basic", include_response = FALSE)

  expect_equal(coef(m_z), par_z1$Std_Coefficient, ignore_attr = TRUE)
  expect_equal(par_z1$Std_Coefficient[-1], par_z2$Std_Coefficient[-1])
  expect_equal(par_z0$Std_Coefficient * sd(iris$Sepal.Length), par_z2$Std_Coefficient)

  # glm ---
  m <- glm(am ~ mpg, mtcars, family = binomial())
  expect_equal(
    standardize_parameters(m),
    standardize_parameters(m, include_response = FALSE),
    ignore_attr = TRUE
  )
})


test_that("include_response | parameters", {
  data(iris)
  iris$Sepal.Length <- iris$Sepal.Length * 5
  m <<- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris)

  # parameters ---
  pars <- model_parameters(m, effects = "fixed")
  pars_z0 <- standardize_parameters(pars, method = "basic")
  pars_z1 <- standardize_parameters(pars, method = "basic", include_response = FALSE)
  expect_equal(pars_z0$Std_Coefficient[-1] * sd(iris$Sepal.Length), pars_z1$Std_Coefficient[-1])

  # boot ---
  skip_if_not_installed("boot")
  pars <- bootstrap_parameters(m)
  pars_z0 <- standardize_parameters(pars, method = "basic")
  pars_z1 <- standardize_parameters(pars, method = "basic", include_response = FALSE)
  expect_equal(pars_z0$Std_Coefficient[-1] * sd(iris$Sepal.Length), pars_z1$Std_Coefficient[-1])
})


test_that("include_response | bayes", {
  skip_if_not_installed("rstanarm")
  skip_on_cran()

  data(iris)
  iris$Sepal.Length <- iris$Sepal.Length * 5
  m <- rstanarm::stan_glm(Sepal.Length ~ Petal.Length + Petal.Width, data = iris, refresh = 0)

  expect_warning(m_z <- datawizard::standardize(m, include_response = FALSE))
  expect_warning(par_z1 <- standardize_posteriors(m, include_response = FALSE))
  par_z0 <- standardize_posteriors(m, method = "basic")
  par_z2 <- standardize_posteriors(m, method = "basic", include_response = FALSE)

  expect_equal(sapply(insight::get_parameters(m_z), mean), sapply(par_z1, mean), tolerance = 0.1)
  expect_equal(sapply(par_z1, mean)[-1], sapply(par_z2, mean)[-1], tolerance = 0.1)
  expect_equal(sapply(par_z0, mean) * sd(iris$Sepal.Length), sapply(par_z2, mean), tolerance = 0.1)
})
