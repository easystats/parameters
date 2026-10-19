skip_on_cran()
skip_if_not_installed("boot")

test_that("bootstrap_parameters.bootstrap_model", {
  data(iris)
  m_draws <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
  set.seed(123)
  draws <- bootstrap_model(m_draws)
  draws$lin_comb <- draws$Sepal.Width - draws$Petal.Length
  out <- bootstrap_parameters(draws)
  expect_snapshot(print(out))
})

test_that("bootstrap_model intercept-only", {
  y <- 1:10
  mod <- lm(y ~ 1)
  set.seed(123)
  out <- bootstrap_model(mod, iterations = 20)
  expect_equal(
    as.numeric(out),
    c(
      6.3, 4.8, 7, 6, 6.3, 6.7, 6.6, 6.6, 6.1, 6.1, 6.3, 6.3, 6.1,
      7.2, 5.9, 5.6, 5.7, 6.8, 6.7, 6.4
    ),
    tolerance = 1e-2
  )
})

test_that("bootstrap_model semiparametric for merMod", {
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  m <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  set.seed(123)
  out <- bootstrap_model(m, iterations = 20, type = "semiparametric")
  expect_named(out, c("(Intercept)", "Days"))
  expect_identical(nrow(out), 20L)
  set.seed(123)
  out <- bootstrap_parameters(m, iterations = 20, type = "semiparametric")
  expect_identical(out$Parameter, c("(Intercept)", "Days"))
})

test_that("bootstrap_model resamples clusters for clmm", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  m <- ordinal::clmm(rating ~ temp + contact + (1 | judge), data = wine)
  .expect_cluster_bootstrap(m, wine, "judge")

  set.seed(42)
  draws <- bootstrap_model(m, iterations = 20)
  set.seed(42)
  out <- model_parameters(m, bootstrap = TRUE, iterations = 20, verbose = FALSE)
  expected <- bootstrap_parameters(draws)
  expect_equal(out$Coefficient, expected$Coefficient, tolerance = 1e-10)
  expect_equal(out$CI_low, expected$CI_low, tolerance = 1e-10)
  expect_equal(out$CI_high, expected$CI_high, tolerance = 1e-10)
})

test_that("bootstrap_model resamples clusters for lme", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  skip_if_not_installed("withr")
  withr::local_package("nlme")
  data(sleepstudy, package = "lme4")
  m1 <- nlme::lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
  .expect_cluster_bootstrap(m1, sleepstudy, "Subject")
  .expect_cluster_bootstrap(m1, sleepstudy, "Subject", sim = "balanced")
  m2 <- nlme::lme(Reaction ~ Days, random = ~ Days | Subject, data = sleepstudy)
  .expect_cluster_bootstrap(m2, sleepstudy, "Subject")
})

test_that("bootstrap_model cluster draws match the mean of cluster means", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("withr")
  withr::local_package("nlme")
  set.seed(2)
  d <- data.frame(
    g1 = factor(rep(1:6, each = 8)),
    g2 = factor(rep(rep(1:2, each = 4), times = 6))
  )
  d$y <- rnorm(6)[d$g1] + rnorm(12)[interaction(d$g1, d$g2)] + rnorm(48)
  # Each g1 cluster has 8 rows, and each g2 group within it has 4 rows. The
  # covariance matrix of y then has identical blocks for the g1 clusters, and
  # the generalized least squares weights are equal for all rows. Thus the
  # intercept of an intercept-only model is the mean of the g1 cluster means,
  # and this holds for every draw, because a draw also has equal cluster sizes.
  cluster_means <- tapply(d$y, d$g1, mean)
  clusters <- unique(as.character(d$g1))

  models <- list(
    nlme::lme(y ~ 1, random = ~ 1 | g1, data = d),
    nlme::lme(y ~ 1, random = ~ 1 | g1 / g2, data = d)
  )
  for (m in models) {
    set.seed(7)
    out <- bootstrap_model(m, iterations = 20)
    set.seed(7)
    draws <- boot::boot(seq_along(clusters), function(x, i) i, R = 20)$t
    expected <- apply(draws, 1, function(i) mean(cluster_means[clusters[i]]))
    expect_identical(nrow(out), 20L)
    expect_equal(out[["(Intercept)"]], unname(expected), tolerance = 1e-8)
  }
})

test_that("bootstrap_model errors for grouping factors that do not nest", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  m <- ordinal::clmm(rating ~ temp + (1 | judge) + (1 | bottle), data = wine)
  expect_error(bootstrap_model(m, iterations = 5), regex = "`judge`.*`bottle`")

  # panels of judges, with judge 3 in two panels
  wine$panel <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3)[wine$judge])
  wine$panel[wine$judge == "3" & wine$temp == "warm"] <- "2"
  m <- ordinal::clmm(rating ~ temp + (1 | panel) + (1 | judge), data = wine)
  expect_error(bootstrap_model(m, iterations = 5), regex = "`panel`.*`judge`")
})

test_that("bootstrap_model resamples the factor that others nest in by data", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  # panels of three judges each, as separate terms of the formula; a panel that
  # is drawn twice needs new judge labels too
  wine$panel <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3)[wine$judge])
  m <- ordinal::clmm(rating ~ temp + (1 | panel) + (1 | judge), data = wine)
  .expect_cluster_bootstrap(m, wine, "panel", relabel = c("panel", "judge"))
})

test_that("bootstrap_model resamples the factor with the most clusters", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  # the cells of judge:temp nest in both factors; judge has 9 levels and temp 2,
  # and temp keeps its labels, because the term also has judge
  m <- ordinal::clmm(rating ~ temp + (1 | judge:temp), data = wine)
  .expect_cluster_bootstrap(m, wine, "judge")
})

test_that("bootstrap_model resamples a factor that is not a fixed effect", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  # judge (9 levels) and temp (2 levels) both qualify, but judge is a fixed
  # effect here
  m <- ordinal::clmm(rating ~ judge + (1 | judge:temp), data = wine)
  groups <- .bootstrap_cluster_groups(m, insight::get_data(m, verbose = FALSE))
  expect_identical(groups$top, "temp")
  expect_identical(groups$relabel, "temp")
})

test_that("bootstrap_model resamples a factor that others nest in through a middle factor", {
  skip_if_not_installed("ordinal")
  # 12 teachers in 4 schools, and students 1 to 5 in each teacher's class; the
  # formula nests student in teacher, and the data nest teacher in school
  set.seed(5)
  d <- expand.grid(obs = 1:2, student = factor(1:5), teacher = factor(1:12))
  d$school <- factor((as.numeric(d$teacher) - 1) %/% 3 + 1)
  d$x <- rnorm(nrow(d))
  latent <- d$x + rnorm(4)[d$school] + rnorm(12)[d$teacher] + rnorm(nrow(d))
  d$rating <- cut(latent, c(-Inf, -1, 0, 1, Inf), ordered_result = TRUE)
  m <- ordinal::clmm(rating ~ x + (1 | school) + (1 | teacher / student), data = d)
  .expect_cluster_bootstrap(m, d, "school", relabel = c("school", "teacher", "student"))
})

test_that("bootstrap_model keeps nested grouping factors that are fixed effects", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("withr")
  withr::local_package("nlme")
  set.seed(3)
  d <- data.frame(
    subject = factor(rep(1:10, each = 4)),
    cond = factor(rep(c("a", "b"), 20))
  )
  d$y <- rnorm(10)[d$subject] + (d$cond == "b") + rnorm(40)
  m <- nlme::lme(y ~ cond, random = ~ 1 | subject / cond, data = d)
  .expect_cluster_bootstrap(m, d, "subject")
  set.seed(42)
  expect_identical(nrow(bootstrap_model(m, iterations = 20)), 20L)
})

test_that("bootstrap_model errors for relabelled grouping factors that are fixed effects", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  d <- sleepstudy
  d$grp <- factor(as.numeric(d$Subject) > 9)
  m <- nlme::lme(Reaction ~ Days + grp, random = ~ 1 | grp / Subject, data = d)
  expect_error(bootstrap_model(m, iterations = 5), regex = "labels in `grp`")
})

test_that("bootstrap_model errors if refits do not return the parameters", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  m <- ordinal::clmm(rating ~ temp + (1 | judge), data = wine)
  get_parameters <- insight::get_parameters
  # rename the parameters of refitted models from the n-th refit on: the first
  # refit is on all clusters, and later ones are the draws
  rename_from <- function(n) {
    refits <- 0
    function(x, ...) {
      out <- get_parameters(x, ...)
      if (!identical(x, m)) {
        refits <<- refits + 1
        if (refits >= n) {
          out$Parameter <- paste0(out$Parameter, "_x")
        }
      }
      out
    }
  }
  local_mocked_bindings(get_parameters = rename_from(1), .package = "insight")
  expect_error(
    bootstrap_model(m, iterations = 3),
    regex = "does not return the parameters"
  )
  local_mocked_bindings(get_parameters = rename_from(2), .package = "insight")
  expect_error(
    bootstrap_model(m, iterations = 3),
    regex = "None of the bootstrap samples"
  )
})

test_that("bootstrap_model errors for permutation type in mixed models", {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal")
  m <- ordinal::clmm(rating ~ temp + (1 | judge), data = wine)
  expect_error(
    bootstrap_model(m, iterations = 5, type = "permutation"),
    regex = "\"permutation\""
  )
})

test_that("bootstrap_model shows the error if the refit on all clusters fails", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  skip_if("package:nlme" %in% search(), "nlme is attached")
  data(sleepstudy, package = "lme4")
  # without nlme attached, update() cannot find `lme.formula()`
  m <- nlme::lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
  expect_error(bootstrap_model(m, iterations = 2), regex = "lme.formula")
})

test_that("bootstrap_model resamples rows for models without clusters", {
  m <- lm(mpg ~ wt + hp, data = mtcars)
  set.seed(42)
  out <- bootstrap_model(m, iterations = 20)
  set.seed(42)
  draws <- boot::boot(seq_len(nrow(mtcars)), function(x, i) i, R = 20)$t
  expected <- t(apply(draws, 1, function(i) {
    stats::coef(stats::lm(mpg ~ wt + hp, data = mtcars[i, ]))
  }))
  expect_equal(unname(as.matrix(out)), unname(expected), tolerance = 1e-10)
})

test_that("bootstrap_model semiparametric errors for glmmTMB", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  m <- glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  expect_error(
    bootstrap_model(m, iterations = 5, type = "semiparametric"),
    regex = "not available for models from"
  )
})

# refit the model to responses drawn with simulate(), one draw per iteration
.parametric_draws <- function(model, data, response, iterations) {
  t(vapply(
    seq_len(iterations),
    function(i) {
      d <- data
      d[[response]] <- stats::simulate(model, nsim = 1)[[1]]
      stats::coef(stats::update(model, data = d))
    },
    numeric(length(stats::coef(model)))
  ))
}

test_that("bootstrap_model parametric refits to simulated responses", {
  m <- lm(mpg ~ wt + hp, data = mtcars)
  set.seed(42)
  out <- bootstrap_model(m, iterations = 50, type = "parametric")
  set.seed(42)
  expected <- .parametric_draws(m, mtcars, "mpg", 50)
  expect_named(out, c("(Intercept)", "wt", "hp"))
  expect_equal(unname(as.matrix(out)), unname(expected), tolerance = 1e-10)

  m <- glm(carb ~ wt, family = poisson, data = mtcars)
  set.seed(42)
  out <- bootstrap_model(m, iterations = 50, type = "parametric")
  set.seed(42)
  expected <- .parametric_draws(m, mtcars, "carb", 50)
  expect_named(out, c("(Intercept)", "wt"))
  expect_equal(unname(as.matrix(out)), unname(expected), tolerance = 1e-10)
})

test_that("bootstrap_model parametric draws match the lm sampling distribution", {
  # simulate.lm() draws y* = X b + e with e ~ N(0, sigma^2), so the refitted
  # coefficients have covariance sigma^2 (X'X)^-1, which is vcov(m)
  m <- lm(mpg ~ wt + hp, data = mtcars)
  set.seed(123)
  out <- bootstrap_model(m, iterations = 1000, type = "parametric")
  se <- sqrt(diag(stats::vcov(m)))
  expect_equal(unname(apply(out, 2, stats::sd) / se), rep(1, 3), tolerance = 0.1)
  expect_lt(max(abs(colMeans(out) - stats::coef(m)) / se), 0.2)
})

test_that("bootstrap_parameters parametric returns a row per coefficient", {
  m <- lm(mpg ~ wt + hp, data = mtcars)
  set.seed(42)
  out <- bootstrap_parameters(m, iterations = 50, type = "parametric")
  expect_identical(out$Parameter, c("(Intercept)", "wt", "hp"))
  expect_false(anyNA(out$Coefficient))
  expect_false(anyNA(out$CI_low))
  expect_false(anyNA(out$CI_high))
})

# the error message wraps at the console width, so spaces in `pattern` match
# any white space
.expect_parametric_error <- function(model, pattern) {
  expect_error(
    bootstrap_model(model, iterations = 5, type = "parametric"),
    regexp = gsub(" ", "\\s+", pattern, fixed = TRUE)
  )
}

test_that("bootstrap_model parametric errors for models without usable simulate()", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("nlme")
  data(wine, package = "ordinal")
  data(Orthodont, package = "nlme")
  use_ordinary <- ".*Use `type = \"ordinary\"` instead"

  .expect_parametric_error(
    ordinal::clm(rating ~ temp, data = wine),
    paste0("`simulate\\(\\)` does not work for models of class `clm`", use_ordinary)
  )
  .expect_parametric_error(
    ordinal::clmm(rating ~ temp + (1 | judge), data = wine),
    paste0("`simulate\\(\\)` does not work for models of class `clmm`", use_ordinary)
  )
  .expect_parametric_error(
    nlme::lme(distance ~ age, random = ~ 1 | Subject, data = Orthodont),
    paste0(
      "`simulate\\(\\)` does not return one numeric response per row for models of class `lme`",
      use_ordinary
    )
  )
  .expect_parametric_error(
    glm(carb ~ wt, family = quasipoisson, data = mtcars),
    paste0(
      "`simulate\\(\\)` does not work for models of class `glm` \\(family 'quasipoisson' not implemented\\)",
      use_ordinary
    )
  )
})

test_that("bootstrap_model parametric errors if simulate() and the data differ in rows", {
  m <- lm(Ozone ~ Wind, data = airquality, na.action = na.exclude)
  .expect_parametric_error(
    m,
    "`simulate\\(\\)` returns 153 responses, but the model data has 116 rows.*Use `type = \"ordinary\"` instead"
  )
  # rows with missing values that are dropped from both are fine
  set.seed(42)
  out <- bootstrap_model(
    lm(Ozone ~ Wind, data = airquality),
    iterations = 5,
    type = "parametric"
  )
  expect_shape(out, dim = c(5L, 2L))
})

test_that("bootstrap_model parametric gives no warning if no seed exists yet", {
  skip_if_not_installed("ordinal")
  skip_if_not_installed("withr")
  data(wine, package = "ordinal")
  m <- ordinal::clm(rating ~ temp, data = wine)
  withr::with_preserve_seed({
    rm(".Random.seed", envir = globalenv())
    expect_no_warning(.expect_parametric_error(
      m,
      "does not work for models of class `clm`"
    ))
    expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  })
})

test_that("bootstrap_model parametric errors for responses that are not a data column", {
  skip_if_not_installed("lme4")
  data(cbpp, package = "lme4")
  use_ordinary <- ".*Use `type = \"ordinary\"` instead"

  .expect_parametric_error(
    lm(log(mpg) ~ wt, data = mtcars),
    paste0("response `log\\(mpg\\)` is not one column of the data", use_ordinary)
  )
  .expect_parametric_error(
    lm(scale(mpg) ~ wt, data = mtcars),
    paste0("response `scale\\(mpg\\)` is not one column of the data", use_ordinary)
  )
  .expect_parametric_error(
    glm(cbind(incidence, size - incidence) ~ period, family = binomial, data = cbpp),
    paste0(
      "response `cbind\\(incidence, size - incidence\\)` is not one column of the data",
      use_ordinary
    )
  )
})
