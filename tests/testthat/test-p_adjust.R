skip_on_cran()


test_that("model_parameters, p-adjust", {
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
  expect_equal(mp$p, c(0, 0.03225, 0.21714, 0.99748, 0.09875, 0.18822), tolerance = 1e-3)
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
    mp <- model_parameters(model, include_info = TRUE, keep = c("wt", "hp"), p_adjust = "bonferroni"),
    "more than 1 element"
  )
  expect_equal(
    mp[["p"]],
    p.adjust(coef(summary(model))[c(2, 5), 4], "bonferroni"),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  expect_message(
    mp <- model_parameters(model, include_info = TRUE, keep = c("cyl", "gear"), p_adjust = "bonferroni"),
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
  skip_if_not_installed("emmeans")
  m <- pairs(emmeans::emmeans(aov(Sepal.Width ~ Species, data = iris), ~Species))
  mp <- model_parameters(m)
  expect_equal(mp$p, as.data.frame(m)$p.value, tolerance = 1e-4)

  m <- pairs(emmeans::emmeans(aov(Sepal.Width ~ Species, data = iris), ~Species), adjust = "scheffe")
  mp <- model_parameters(m, p_adjust = "scheffe")
  expect_equal(mp$p, as.data.frame(m)$p.value, tolerance = 1e-4)
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
  expect_equal(mp$CI_low, c(67.70195, -0.48377, -4.66802, -7.51974, 8.42651, 11.50991), tolerance = 1e-3)

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
      0.56769, 0.57466, 0.98029, 0.83123, 0.22681, 0.06271, 0.99876,
      0.00068, 0.61786, 0.95269, 0.81296, 0.60973, 0.97504, 0.80566,
      0.81871, 0.00024, NA, NA
    ),
    tolerance = 1e-3
  )
  expect_equal(
    mp$CI_low,
    c(
      -1.6935, -2.6841, -0.45839, -1.30244, -0.14911, -0.01933, -0.76516,
      0.4494, -0.77256, -2.41501, -3.08449, -0.87083, -2.50859, -2.91223,
      -8.38616, -4.18299, 0.92944, 0.16671
    ),
    tolerance = 1e-3)
})
