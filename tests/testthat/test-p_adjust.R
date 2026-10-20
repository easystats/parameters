skip_on_cran()


test_that("model_parameters, p-adjust", {
  data(mtcars)
  model <- lm(mpg ~ wt * cyl + am + log(hp), data = mtcars)
  mp <- model_parameters(model)
  expect_equal(mp$p, c(0, 0.00304, 0.02765, 0.65851, 0.01068, 0.02312), tolerance = 1e-3)
  mp <- model_parameters(model, p_adjust = "BH")
  expect_equal(mp$p, c(0, 0.00912, 0.03318, 0.65851, 0.02137, 0.03318), tolerance = 1e-3)
  mp <- model_parameters(model, p_adjust = "bonferroni")
  expect_equal(mp$p, c(0, 0.01824, 0.16588, 1, 0.06411, 0.13869), tolerance = 1e-3)
  mp <- model_parameters(model, p_adjust = "scheffe")
  expect_equal(mp$p, c(0, 0.1425, 0.50499, 0.99981, 0.30911, 0.46396), tolerance = 1e-3)
  mp <- model_parameters(model, p_adjust = "tukey")
  expect_equal(mp$p, c(0, 0.00304, 0.02765, 0.65851, 0.01068, 0.02312), tolerance = 1e-3)
  mp <- model_parameters(model, p_adjust = "sidak")
  expect_equal(mp$p, c(0, 0.0181, 0.15483, 0.99841, 0.06242, 0.13092), tolerance = 1e-3)
})


test_that("model_parameters, p-adjust after keep/drop", {
  model <- lm(mpg ~ wt + cyl + gear + hp, data = mtcars)
  mp <- model_parameters(model, p_adjust = "bonferroni")
  expect_equal(
    mp[["p"]],
    p.adjust(coef(summary(model))[, 4], "bonferroni"),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  expect_message(
    mp <- model_parameters(
      model,
      include_info = TRUE,
      keep = c("wt", "hp"),
      p_adjust = "bonferroni"
    ),
    "more than 1 element"
  )
  expect_equal(
    mp[["p"]],
    p.adjust(coef(summary(model))[c(2, 5), 4], "bonferroni"),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  expect_message(
    mp <- model_parameters(
      model,
      include_info = TRUE,
      keep = c("cyl", "gear"),
      p_adjust = "bonferroni"
    ),
    "more than 1 element"
  )
  expect_equal(
    mp[["p"]],
    p.adjust(coef(summary(model))[3:4, 4], "bonferroni"),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})


test_that("model_parameters, emmeans, p-adjust", {
  data(iris)
  skip_if_not_installed("emmeans")
  m <- pairs(emmeans::emmeans(aov(Sepal.Width ~ Species, data = iris), ~Species))
  mp <- model_parameters(m)
  expect_equal(mp$p, as.data.frame(m)$p.value, tolerance = 1e-4)

  m <- pairs(
    emmeans::emmeans(aov(Sepal.Width ~ Species, data = iris), ~Species),
    adjust = "scheffe"
  )
  mp <- model_parameters(m, p_adjust = "scheffe")
  expect_equal(mp$p, as.data.frame(m)$p.value, tolerance = 1e-4)

  m <- pairs(
    emmeans::emmeans(aov(Sepal.Width ~ Species, data = iris), ~Species),
    adjust = "tukey"
  )
  mp <- model_parameters(m, p_adjust = "tukey")
  expect_equal(mp$p, as.data.frame(m)$p.value, tolerance = 1e-4)

  data(warpbreaks)
  warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)

  myc <- pairs(emmeans::emmeans(warp.lm, ~ wool + tension))
  mp <- model_parameters(myc, p_adjust = "tukey")
  expect_equal(mp$p, as.data.frame(myc)$p.value, tolerance = 1e-4)

  skip_if(getRversion() < "4.5.0")
  data(penguins)
  m <- lm(bill_len ~ sex * species + island, data = penguins)
  emm <- pairs(emmeans::emmeans(m, ~ sex + species))
  mp <- model_parameters(emm, p_adjust = "tukey")
  expect_equal(mp$p, as.data.frame(emm)$p.value, tolerance = 1e-4)
})


test_that("model_parameters, p-adjust, method names in any letter case", {
  methods <- c(bh = "BH", by = "BY", Holm = "holm")

  model <- lm(mpg ~ wt + cyl + gear + hp, data = mtcars)
  p_raw <- coef(summary(model))[, 4]
  for (method in names(methods)) {
    mp <- model_parameters(model, p_adjust = method)
    expect_equal(
      mp$p,
      stats::p.adjust(p_raw, methods[[method]]),
      tolerance = 1e-10,
      ignore_attr = TRUE
    )
  }
  expect_silent(model_parameters(model, p_adjust = "NONE"))
  mp <- model_parameters(model, p_adjust = "NONE")
  expect_equal(mp$p, p_raw, tolerance = 1e-10, ignore_attr = TRUE)
  out <- capture.output(print(mp))
  expect_true(any(startsWith(out, "Parameter")))
  expect_false(any(grepl("p-value adjustment method", out, fixed = TRUE)))

  skip_if_not_installed("emmeans")
  m <- lm(mpg ~ factor(cyl) + hp, data = mtcars)
  pw <- pairs(emmeans::emmeans(m, ~cyl))
  p_raw <- summary(pw, adjust = "none")$p.value
  for (method in names(methods)) {
    mp <- model_parameters(pw, p_adjust = method)
    expect_equal(mp$p, stats::p.adjust(p_raw, methods[[method]]), tolerance = 1e-10)
  }
})


test_that("model_parameters, emmeans, p-adjust mvt", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("mvtnorm")

  m <- lm(mpg ~ factor(cyl) + hp, data = mtcars)
  pw <- pairs(emmeans::emmeans(m, ~cyl))
  set.seed(123)
  mp <- model_parameters(pw, p_adjust = "mvt")
  set.seed(123)
  p_mvt <- summary(pw, adjust = "mvt", infer = c(FALSE, TRUE))$p.value
  expect_equal(mp$p, p_mvt, tolerance = 1e-10)
  mp_none <- model_parameters(pw, p_adjust = "none")
  expect_equal(mp$CI_low, mp_none$CI_low, tolerance = 1e-10)
  expect_equal(mp$CI_high, mp_none$CI_high, tolerance = 1e-10)
  out <- capture.output(print(mp))
  expect_true(any(grepl("p-value adjustment method: Multivariate t", out, fixed = TRUE)))

  # each "by" group is a separate family
  w <- lm(breaks ~ wool * tension, data = warpbreaks)
  pw_by <- pairs(emmeans::emmeans(w, ~ tension | wool))
  set.seed(123)
  mp <- model_parameters(pw_by, p_adjust = "mvt")
  set.seed(123)
  p_mvt <- summary(pw_by, adjust = "mvt", infer = c(FALSE, TRUE))$p.value
  expect_equal(mp$p, p_mvt, tolerance = 1e-10)
  # balanced design, all pairwise comparisons: mvt and Tukey agree
  p_tukey <- summary(pw_by, adjust = "tukey")$p.value
  expect_lt(max(abs(mp$p - p_tukey)), 1e-3)

  # estimated means, which by default have no p-values in emmeans
  m2 <- lm(I(mpg - 20) ~ factor(cyl), data = mtcars)
  emm <- emmeans::emmeans(m2, ~cyl)
  set.seed(123)
  mp <- model_parameters(emm, p_adjust = "mvt")
  set.seed(123)
  p_mvt <- summary(emm, adjust = "mvt", infer = c(FALSE, TRUE))$p.value
  expect_equal(mp$p, p_mvt, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(mp$p, model_parameters(emm)$p)))
})


test_that("model_parameters, emmeans, p-adjust mvt stored in the object", {
  skip_if_not_installed("emmeans")
  skip_if_not_installed("mvtnorm")

  m <- lm(mpg ~ factor(cyl) + hp, data = mtcars)
  pw <- pairs(emmeans::emmeans(m, ~cyl), adjust = "mvt")
  expect_no_message(model_parameters(pw))
  set.seed(123)
  mp <- model_parameters(pw)
  set.seed(123)
  p_mvt <- summary(pw, adjust = "mvt", infer = c(FALSE, TRUE))$p.value
  expect_equal(mp$p, p_mvt, tolerance = 1e-10)

  el <- emmeans::emmeans(m, pairwise ~ cyl, adjust = "mvt")
  expect_no_message(model_parameters(el))
  mp <- model_parameters(el)
  p_mvt <- summary(el$contrasts, adjust = "mvt", infer = c(FALSE, TRUE))$p.value
  expect_lt(max(abs(mp$p[mp$Component == "contrasts"] - p_mvt)), 1e-3)
})


test_that("model_parameters, p-adjust, unsupported methods", {
  model <- lm(mpg ~ wt + cyl + gear + hp, data = mtcars)
  mp_none <- model_parameters(model)

  expect_message(
    model_parameters(model, p_adjust = "mvt"),
    "only available for objects from package `emmeans`",
    fixed = TRUE
  )
  mp <- suppressMessages(model_parameters(model, p_adjust = "mvt"))
  expect_equal(mp$p, mp_none$p, tolerance = 1e-10)
  out <- capture.output(print(mp))
  expect_true(any(startsWith(out, "Parameter")))
  expect_false(any(grepl("p-value adjustment method", out, fixed = TRUE)))

  expect_message(
    model_parameters(model, p_adjust = "foo"),
    "`p_adjust` must be one of",
    fixed = TRUE
  )
  mp <- suppressMessages(model_parameters(model, p_adjust = "foo"))
  out <- capture.output(print(mp))
  expect_true(any(startsWith(out, "Parameter")))
  expect_false(any(grepl("p-value adjustment method", out, fixed = TRUE)))

  skip_if_not_installed("emmeans")
  m <- lm(mpg ~ factor(cyl) + hp, data = mtcars)
  pw <- pairs(emmeans::emmeans(m, ~cyl))
  expect_message(
    model_parameters(pw, p_adjust = "foo"),
    "`p_adjust` must be one of",
    fixed = TRUE
  )
  mp <- suppressMessages(model_parameters(pw, p_adjust = "foo"))
  out <- capture.output(print(mp))
  expect_true(any(startsWith(out, "contrast")))
  expect_false(any(grepl("p-value adjustment method", out, fixed = TRUE)))
})


test_that("model_parameters, simultaenous confidence intervals", {
  skip_if_not_installed("mvtnorm")
  m <- lm(mpg ~ wt + hp, data = mtcars)
  set.seed(123)
  out <- model_parameters(m, p_adjust = "sup-t")
  expect_snapshot(print(out, zap_small = TRUE))

  skip_if_not_installed("lme4")
  data("qol_cancer")
  qol_cancer <- cbind(
    qol_cancer,
    demean(qol_cancer, select = c("phq4", "QoL"), by = "ID")
  )
  model <- lme4::lmer(
    QoL ~ time + phq4_within + phq4_between + (1 | ID),
    data = qol_cancer
  )
  set.seed(123)
  mp <- model_parameters(model, p_adjust = "sup-t")
  expect_equal(mp$p, c(0, 0.27904, 0, 0, NA, NA), tolerance = 1e-3)
  expect_equal(
    mp$CI_low,
    c(67.70195, -0.48377, -4.66802, -7.51974, 8.42651, 11.50991),
    tolerance = 1e-3
  )

  skip_if_not_installed("glmmTMB")
  data("Salamanders", package = "glmmTMB")
  model <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ spp + mined + (1 | site),
    ziformula = ~ spp + mined,
    family = glmmTMB::nbinom2,
    data = Salamanders
  ))
  set.seed(123)
  mp <- model_parameters(model, p_adjust = "sup-t")
  expect_equal(
    mp$p,
    c(
      0.56769,
      0.57466,
      0.98029,
      0.83123,
      0.22681,
      0.06271,
      0.99876,
      0.00068,
      0.61786,
      0.95269,
      0.81296,
      0.60973,
      0.97504,
      0.80566,
      0.81871,
      0.00024,
      NA,
      NA
    ),
    tolerance = 1e-3
  )
  expect_equal(
    mp$CI_low,
    c(
      -1.6935,
      -2.6841,
      -0.45839,
      -1.30244,
      -0.14911,
      -0.01933,
      -0.76516,
      0.4494,
      -0.77256,
      -2.41501,
      -3.08449,
      -0.87083,
      -2.50859,
      -2.91223,
      -8.38616,
      -4.18299,
      0.92944,
      0.16671
    ),
    tolerance = 1e-3
  )
})
