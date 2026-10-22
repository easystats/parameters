skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
skip_if_not_installed("rstanarm")

test_that("marginaleffects()", {
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  model <- marginaleffects::avg_slopes(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  out <- model_parameters(model)
  expect_identical(nrow(out), 1L)
  cols <- c(
    "Parameter",
    "Comparison",
    "Coefficient",
    "SE",
    "Statistic",
    "p",
    "S",
    "CI",
    "CI_low",
    "CI_high"
  )
  expect_true(all(cols %in% colnames(out)))
  out <- model_parameters(model, exponentiate = TRUE)
  expect_equal(out$Coefficient, 1.394, tolerance = 1e-3)

  # Bayesian
  x <- suppressWarnings(rstanarm::stan_glm(
    Sepal.Width ~ Species * Petal.Length,
    data = iris,
    refresh = 0,
    iter = 100,
    chains = 1
  ))
  model <- marginaleffects::avg_slopes(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(parameters(model)), 1L)

  # remove redundant columns
  skip_if_not_installed("mgcv")
  data(iris)
  model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
  mfx <- marginaleffects::avg_slopes(model, variables = "Petal.Length")
  out <- model_parameters(mfx)
  expect_identical(dim(out), c(1L, 10L))
  expect_named(
    out,
    c(
      "Parameter",
      "Comparison",
      "Coefficient",
      "SE",
      "Statistic",
      "p",
      "S",
      "CI",
      "CI_low",
      "CI_high"
    )
  )
  mfx <- marginaleffects::avg_slopes(model, variables = "Petal.Length", by = "Species")
  out <- model_parameters(mfx)
  expect_identical(dim(out), c(3L, 11L))
  expect_named(
    out,
    c(
      "Parameter",
      "Comparison",
      "Species",
      "Coefficient",
      "SE",
      "Statistic",
      "p",
      "S",
      "CI",
      "CI_low",
      "CI_high"
    )
  )
})


test_that("predictions()", {
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  p <- marginaleffects::avg_predictions(x, by = "Species")
  out <- model_parameters(p)
  expect_identical(nrow(out), 3L)
  expect_named(
    out,
    c(
      "Predicted",
      "Species",
      "SE",
      "CI",
      "CI_low",
      "CI_high",
      "S",
      "Statistic",
      "df",
      "p"
    )
  )
  out <- model_parameters(p, exponentiate = TRUE)
  expect_equal(out$Predicted, c(30.81495, 15.95863, 19.57004), tolerance = 1e-4)
})


test_that("comparisons()", {
  data(iris)
  # Frequentist
  x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
  m <- marginaleffects::avg_comparisons(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(model_parameters(m)), 1L)
  out <- model_parameters(m, exponentiate = TRUE)
  expect_equal(out$Coefficient, 1.393999, tolerance = 1e-4)

  # Bayesian
  x <- suppressWarnings(rstanarm::stan_glm(
    Sepal.Width ~ Species * Petal.Length,
    data = iris,
    refresh = 0,
    iter = 100,
    chains = 1
  ))
  m <- marginaleffects::avg_slopes(
    x,
    newdata = insight::get_datagrid(x, by = "Species"),
    variables = "Petal.Length"
  )
  expect_identical(nrow(parameters(m)), 1L)
})


test_that("hypotheses()", {
  data(mtcars)
  x <- lm(mpg ~ hp + wt, data = mtcars)
  m <- marginaleffects::hypotheses(x, "hp = wt")
  expect_identical(nrow(model_parameters(m)), 1L)
})


test_that("slopes()", {
  m <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)

  x <- marginaleffects::slopes(
    m,
    variables = "Petal.Length",
    newdata = insight::get_datagrid(m, by = "Species")
  )
  out <- model_parameters(x)
  expect_named(
    out,
    c(
      "rowid",
      "Parameter",
      "Comparison",
      "Coefficient",
      "SE",
      "Statistic",
      "p",
      "S",
      "CI",
      "CI_low",
      "CI_high",
      "Species",
      "Petal.Length",
      "Predicted"
    )
  )
  expect_identical(dim(out), c(3L, 14L))
})


test_that("multiple contrasts: Issue #779", {
  skip_if(getRversion() < "4.0.0")
  data(mtcars)
  mod <- lm(mpg ~ as.factor(gear) * as.factor(cyl), data = mtcars)
  cmp <- suppressWarnings(marginaleffects::comparisons(
    mod,
    variables = c("gear", "cyl"),
    newdata = insight::get_datagrid(mod, by = c("gear", "cyl")),
    cross = TRUE
  ))
  cmp <- suppressWarnings(model_parameters(cmp))
  expect_true("Comparison: gear" %in% colnames(cmp))
  expect_true("Comparison: cyl" %in% colnames(cmp))
})


test_that("model_parameters defaults to FALSE: Issue #916", {
  data(mtcars)
  mod <- lm(mpg ~ wt, data = mtcars)
  pred <- marginaleffects::predictions(
    mod,
    newdata = marginaleffects::datagrid(wt = c(1, 2))
  )
  out1 <- model_parameters(pred)
  out2 <- model_parameters(pred, exponentiate = FALSE)
  expect_equal(out1$Predicted, out2$Predicted, tolerance = 1e-4)
})


test_that("digits and ci_digits for marginaleffects", {
  data(mtcars)
  skip_if(getRversion() < "4.2.0")
  out <- lm(mpg ~ wt, data = mtcars) |>
    marginaleffects::hypotheses(hypothesis = "10*wt = 0") |>
    model_parameters(digits = 1)
  expect_snapshot(out)
})


test_that("preserve columns with same name as reserved words", {
  data(mtcars)
  skip_if(getRversion() < "4.2.0")
  skip_if_not_installed("modelbased")

  set.seed(1234)
  x <- rnorm(200)
  z <- rnorm(200)
  # quadratic relationship
  y <- 2 * x + x^2 + 4 * z + rnorm(200)
  d <- data.frame(x, y, z)
  model <- lm(y ~ x + z, data = d)
  pred <- modelbased::estimate_means(model, c("x", "z"))
  expect_named(pred, c("x", "z", "Mean", "SE", "CI_low", "CI_high", "t", "df"))
})


test_that("predictions, bmrs with special response formula", {
  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  m <- insight::download_model("brms_ipw_1")
  skip_if(is.null(m))

  x <- marginaleffects::avg_predictions(
    m,
    variables = "treatment",
    hypothesis = ~pairwise
  )
  out <- model_parameters(x)
  expect_identical(dim(out), c(1L, 10L))
})


test_that("modelbased, tidiers work", {
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")
  skip_if_not_installed("modelbased", minimum_version = "0.12.0.17")
  skip_if(getRversion() < "4.5.0")

  data(penguins)
  m <- lm(bill_len ~ island * sex + bill_dep + species, data = penguins)

  out <- modelbased::estimate_contrasts(
    m,
    "island",
    by = "sex",
    comparison = ratio ~ pairwise
  )
  expect_named(
    out,
    c("Level1", "Level2", "sex", "Ratio", "SE", "CI_low", "CI_high", "t", "df", "p")
  )
  expect_identical(dim(out), c(6L, 10L))

  datagrid <- insight::get_datagrid(m, by = c("island", "sex"), factors = "all")
  out <- marginaleffects::avg_predictions(
    model = m,
    variables = c("island", "sex"),
    newdata = datagrid,
    hypothesis = ratio ~ pairwise | sex
  )
  params <- model_parameters(out)
  expect_named(
    params,
    c(
      "Parameter",
      "Predicted",
      "SE",
      "CI",
      "CI_low",
      "CI_high",
      "S",
      "Statistic",
      "df",
      "p",
      "sex"
    )
  )
  expect_identical(dim(params), c(6L, 11L))

  out <- modelbased::estimate_contrasts(
    m,
    "island",
    by = "sex",
    comparison = ratio ~ inequality
  )
  expect_named(out, c("sex", "Mean_Ratio", "SE", "CI_low", "CI_high", "z", "p"))
  expect_identical(dim(out), c(2L, 7L))

  datagrid <- insight::get_datagrid(m, by = c("island", "sex"), factors = "all")
  out <- marginaleffects::avg_predictions(
    model = m,
    variables = c("island", "sex"),
    newdata = datagrid,
    hypothesis = ratio ~ pairwise | sex
  )
  out <- marginaleffects::hypotheses(out, hypothesis = ~ I(mean(abs(x))) | sex)
  params <- model_parameters(out)
  expect_named(
    params,
    c("Coefficient", "SE", "Statistic", "p", "S", "CI", "CI_low", "CI_high", "sex")
  )
  expect_identical(dim(params), c(2L, 9L))
})


test_that("predictions, using bayestestR #1063", {
  # Following test may fail on CI, probably due to scoping issues?
  # ── Error (test-marginaleffects.R:179:3): predictions, using bayestestR #1063 ───
  # Error in ``[.data.frame`(data, random_factors)`: undefined columns selected
  # Backtrace:
  #     ▆
  #  1. ├─insight::get_datagrid(m, by = "Days", include_random = TRUE) at test-marginaleffects.R:179:3
  #  2. └─insight:::get_datagrid.default(m, by = "Days", include_random = TRUE)
  #  3.   ├─base::lapply(data[random_factors], as.factor)
  #  4.   ├─data[random_factors]
  #  5.   └─base::`[.data.frame`(data, random_factors)
  ## TODO: check this test locally
  skip("TODO: check this test locally, fails on CI, probably due to scoping issues?")

  skip_on_ci()
  skip_on_cran()
  skip_if_not_installed("curl")
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("brms")
  skip_if_not_installed("marginaleffects", minimum_version = "0.29.0")

  m <- insight::download_model("brms_mixed_3")
  skip_if(is.null(m))

  d <- insight::get_datagrid(m, by = "Days", include_random = TRUE)
  x <- marginaleffects::avg_predictions(
    m,
    newdata = d,
    by = "Days",
    allow_new_levels = TRUE
  )
  out <- model_parameters(x)
  expect_named(
    out,
    c(
      "Median",
      "CI",
      "CI_low",
      "CI_high",
      "pd",
      "ROPE_CI",
      "ROPE_low",
      "ROPE_high",
      "ROPE_Percentage",
      "Days"
    )
  )
})


test_that("predictions, don't remove Type column when it's a variable", {
  skip_on_cran()
  skip_if_not_installed("MASS")

  data(housing, package = "MASS")
  m1 <- MASS::polr(Sat ~ Infl + Type + Cont, data = housing, weights = Freq)
  emm <- marginaleffects::avg_predictions(m1, by = "Type")
  out <- model_parameters(emm)

  expect_named(
    out,
    c(
      "Predicted",
      "Type",
      "SE",
      "CI",
      "CI_low",
      "CI_high",
      "S",
      "Statistic",
      "df",
      "p",
      "group"
    )
  )

  skip_if_not_installed("modelbased")
  out <- modelbased::estimate_means(m1, by = "Type")
  expect_named(
    out,
    c("Type", "Response", "Probability", "SE", "CI_low", "CI_high", "t", "df")
  )
})


test_that("preserve focal terms with same name as reserved words", {
  d <- iris
  d$type <- factor(rep(c("a", "b", "c"), 50))
  d$t <- d$type
  m1 <- lm(Sepal.Width ~ type * Petal.Length, data = d)
  m2 <- lm(Sepal.Width ~ t * Petal.Length, data = d)

  out <- model_parameters(marginaleffects::predictions(m1, by = "type"))
  expect_identical(as.character(out$type), c("a", "b", "c"))
  expect_false("Type" %in% colnames(out))

  out <- model_parameters(marginaleffects::avg_slopes(
    m2,
    variables = "Petal.Length",
    by = "t"
  ))
  expect_identical(as.character(out$t), c("a", "b", "c"))
  expect_false("Statistic.1" %in% colnames(out))

  skip_if_not_installed("modelbased")
  out <- model_parameters(modelbased::get_marginalmeans(m1, "type"))
  expect_identical(as.character(out$type), c("a", "b", "c"))
})


# slopes and predictions with a "by" variable called `name`
.by_variable_calls <- function(name) {
  d <- iris
  d$v <- factor(rep(c("a", "b", "c"), 50))
  colnames(d)[colnames(d) == "v"] <- name
  f <- stats::as.formula(paste0("Sepal.Width ~ `", name, "` * Petal.Length"))
  m <- lm(f, data = d)
  list(
    slopes = marginaleffects::avg_slopes(m, variables = "Petal.Length", by = name),
    predictions = marginaleffects::predictions(m, by = name)
  )
}


test_that("by variables named like output columns get the suffix .1", {
  reference <- lapply(.by_variable_calls("safe_var"), model_parameters)
  clashing <- lapply(.by_variable_calls("p"), model_parameters)
  for (call in names(reference)) {
    out <- as.data.frame(clashing[[call]])
    expected <- as.data.frame(reference[[call]])
    colnames(expected)[colnames(expected) == "safe_var"] <- "p.1"
    expect_identical(colnames(out), colnames(expected), info = call)
    expect_equal(out, expected, ignore_attr = TRUE, info = call)
  }
})


test_that("print with a by variable named p", {
  withr::local_options(width = 300)
  out <- model_parameters(.by_variable_calls("p")$slopes)
  rows <- grep("|", capture.output(print(out)), fixed = TRUE, value = TRUE)
  cells <- lapply(strsplit(rows, "|", fixed = TRUE), trimws)
  header <- cells[[1]]
  body <- cells[-1]
  expect_identical(vapply(body, function(x) x[header == "p.1"], character(1)), c("a", "b", "c"))
  expect_match(
    vapply(body, function(x) x[header == "p"], character(1)),
    "^(< \\.001|> \\.999|0\\.\\d{3})$"
  )
})


test_that("by variables named x and x.1 get unique names", {
  d <- iris
  d$p <- factor(rep(c("a", "b", "c"), 50))
  d$p.1 <- factor(rep(c("x", "y"), 75))
  m <- lm(Sepal.Width ~ p * p.1 * Petal.Length, data = d)
  out <- model_parameters(marginaleffects::avg_slopes(
    m,
    variables = "Petal.Length",
    by = c("p", "p.1")
  ))
  expect_true(is.numeric(out$p))
  expect_identical(sort(unique(as.character(out$p.1))), c("x", "y"))
  expect_identical(sort(unique(as.character(out$p.2))), c("a", "b", "c"))
})


test_that("grouped hypothesis with a by variable named p", {
  d <- iris
  d$p <- factor(rep(c("a", "b", "c"), 50))
  d$g <- factor(rep(c("x", "y"), 75))
  m <- lm(Sepal.Width ~ p * g * Petal.Length, data = d)
  out <- model_parameters(marginaleffects::avg_predictions(
    m,
    by = c("g", "p"),
    hypothesis = ~ pairwise | p
  ))
  expect_true(is.numeric(out$p))
  expect_identical(as.character(out$p.1), c("a", "b", "c"))
})
